library(tidyverse)

# choose settings and prep model folder -----------------------------------
well_sel <- 'rl02'
model_name <- 'warmup_step_proj'

start_date <- ymd('1981-01-01')
end_date <- ymd('2024-12-31')

warmup <- T #include 3 year warmup period prior to calibration?

period_correction <- 'step' #none, mean_center, or step

eval_method <- 'blocked_cv' #sliding_window, growing_window, blocked_cv

rch_model <- 'flex'
rch_function <- 'exponential' #exponential, gamma, or fourparam

climate_radius <- 2

pumping_source <- 'wimas'
pumping_radius <- 4
pumping_disag <- 'even'

river_gage <- NA#'Lecompton'

projections <- T
pumping_scenario <- 'lm'

#create directories for model inputs and outputs
model_dir <- paste0('data/models/',well_sel,'/',model_name,'/')
input_dir <- paste0(model_dir,'in/')
output_dir <- paste0(model_dir,'out/')
output_metrics_dir <- paste0(output_dir,'metrics/')
output_params_dir <- paste0(output_dir,'params/')
output_train_dir <- paste0(output_dir,'train_series/')
output_test_dir <- paste0(output_dir,'test_series/')
output_tf_dir <- paste0(output_dir,'transfer_functions/')
output_proj_dir <- paste0(output_dir,'projections/')
dirs = list(model_dir, input_dir, output_dir, 
            output_metrics_dir, output_params_dir, output_train_dir, output_test_dir, output_tf_dir,
            output_proj_dir)
for(i in 1:length(dirs)){
  dir = dirs[[i]]
  if(!dir.exists(dir)) dir.create(dir, recursive = T)
}

#save settings as csv
input_settings <- tibble(setting = c('well', 'model_name', 'start_date', 'end_date', 'warmup', 'eval_method',
                                     'rch_model', 'rch_function',
                                     'climate_radius', 
                                     'period_correction',
                                     'pumping_source', 'pumping_radius', 'pumping_disag',
                                     'river_gage',
                                     'projections', 'pumping_scenario'),
                         value = c(well_sel, model_name, start_date, end_date, warmup, eval_method,
                                   rch_model, rch_function,
                                   climate_radius, 
                                   period_correction,
                                   pumping_source, pumping_radius, pumping_disag,
                                   river_gage,
                                   projections, pumping_scenario))
write_csv(input_settings, paste0(model_dir, 'input_settings.csv'))


# filter and load in inputs -----------------------------------------------
gwl_fp <- paste0('data/wells/clean/daily/',well_sel,'.csv')
if(period_correction == 'mean_center') gwl_fp <- paste0('data/wells/clean/daily_mean_center/',well_sel,'.csv')
gwl <- read_csv(gwl_fp) %>%
  select(date, gwl) %>%
  filter(date >= start_date & date <= end_date)
write_csv(gwl, paste0(input_dir, 'gwl.csv'))
 
precip <- read_csv('data/climate/gridmet/precip.csv') %>%
  filter(well == well_sel & radius == climate_radius) %>%
  select(date, precip) %>%
  filter(date >= start_date & date <= end_date)
write_csv(precip, paste0(input_dir, 'precip.csv')) 

et <- read_csv('data/climate/gridmet/eta_kc.csv') %>%
  filter(well == well_sel & radius == climate_radius) %>%
  select(date, et = eta) %>%
  filter(date >= start_date & date <= end_date)
write_csv(et, paste0(input_dir, 'et.csv')) 

pump <- read_csv(paste0('data/pumping/',pumping_source,'/daily_',pumping_disag,'.csv')) %>%
  filter(well == well_sel & radius == pumping_radius) %>%
  select(date, pump = amt) %>%
  filter(date >= start_date & date <= end_date)
write_csv(pump, paste0(input_dir, 'pump.csv')) 

if(!is.na(river_gage)){
  stage <- read_csv(paste0('data/river/gages/clean/stage_adj/',river_gage,'.csv')) %>%
    select(date, stage) %>%
    filter(date >= start_date & date <= end_date)
  write_csv(stage, paste0(input_dir, 'stage.csv'))
}

# define calibration/validation sets --------------------------------------
#resample observation series so that each evaluation fold has a similar number of
#observations to calibrate on
resample_freq <- '30 days'
date_resample <- data.frame(date_resample = seq.Date(start_date, end_date, by = resample_freq))
gwl_resample <- left_join(date_resample, gwl, by = join_by(closest(date_resample <= date))) %>%
  select(date, gwl = gwl) %>%
  filter(date != lag(date, default = start_date))
if(warmup == T){
  gwl_resample <- filter(gwl_resample, date >= ymd('1984-01-01'))
}

if(eval_method == 'sliding_window'){
  n_splits <- 5
  
  total_n <- nrow(gwl_resample)
  fold_n <- round(0.7*total_n)
  fold_train_n <- round(0.6*total_n)
  fold_test_n <- round(0.1*total_n)
  
  split_points <- round(seq(from = fold_train_n, to = total_n-fold_test_n, length.out = n_splits))
  eval_dates <- data.frame(
    split_date = gwl_resample$date[split_points],
    start_date = gwl_resample$date[split_points - fold_train_n + 1],
    end_date = gwl_resample$date[split_points + fold_test_n]
  )
  #add in final calibration period that includes everything
  eval_dates[n_splits+1,] <- c(gwl_resample$date[length(gwl_resample$date)], gwl_resample$date[1], gwl_resample$date[length(gwl_resample$date)])

  write_csv(eval_dates, paste0(model_dir, 'eval_settings.csv'))
  
  ggplot(gwl, aes(x = date, y = gwl)) + geom_point() +
    geom_vline(xintercept = eval_dates$split_date, color = 'red')
}

if(eval_method == 'blocked_cv'){
  n_blocks <- 5
  
  split_points <- round(seq(from = 1, to = nrow(gwl_resample), length.out = n_blocks+1))
  eval_dates <- data.frame(
    holdout_start = gwl_resample$date[split_points[1:(n_blocks)]],
    holdout_end = gwl_resample$date[split_points[2:(n_blocks+1)]]
  )
  eval_dates$holdout_start[1] <- gwl_resample$date[1]
  eval_dates$holdout_end[n_blocks] <- gwl$date[nrow(gwl)]
  #add in final calibration period that includes everything
  eval_dates[n_blocks+1,] <- c(gwl_resample$date[length(gwl_resample$date)],gwl_resample$date[length(gwl_resample$date)])
  
  write_csv(eval_dates, paste0(model_dir, 'eval_settings.csv'))
  
  ggplot(gwl, aes(x = date, y = gwl)) + geom_point() +
    geom_vline(xintercept = eval_dates$holdout_start, color = 'red')
}

if(eval_method == 'growing_window'){
  n_evals <- 5 
  
  split_points <- round(seq(from = 1, to = nrow(gwl_resample), length.out = n_evals+2))
  eval_dates <- data.frame(
    split_date = gwl_resample$date[split_points[2:(n_evals+1)]],
    end_date = gwl_resample$date[split_points[3:(n_evals+2)]]
  )
  #add in final calibration period that includes everything
  eval_dates[n_evals+1,] <- c(gwl_resample$date[length(gwl_resample$date)],gwl_resample$date[length(gwl_resample$date)])
  
  write_csv(eval_dates, paste0(model_dir, 'eval_settings.csv'))
  
  ggplot(gwl, aes(x = date, y = gwl)) + geom_point() +
    geom_vline(xintercept = eval_dates$split_date, color = 'red')
}


# set up projection time series -------------------------------------------
if(projections){
  proj_dirs <- list.dirs('data/climate/maca', recursive = F)
  
  for(i in 1:length(proj_dirs)){
    proj_dir <- proj_dirs[i]
    climate_scenario <- word(proj_dir, -1, sep = '/')
    
    input_proj_dir <- paste0(input_dir,'projections/',climate_scenario,'/')
    if(!dir.exists(input_proj_dir)) dir.create(input_proj_dir, recursive = T)
    
    proj_precip <- read_csv(paste0(proj_dir,'/precip.csv')) %>%
      filter(well == well_sel & radius == climate_radius) %>%
      select(date, precip) %>%
      filter(date > end_date)
    proj_precip <- rbind(precip, proj_precip)
    write_csv(proj_precip, paste0(input_proj_dir, 'precip.csv')) 
    
    proj_et <- read_csv(paste0(proj_dir,'/eta_kc.csv')) %>%
      filter(well == well_sel & radius == climate_radius) %>%
      select(date, et = eta) %>%
      filter(date > end_date)
    proj_et <- rbind(et, proj_et)
    write_csv(proj_et, paste0(input_proj_dir, 'et.csv')) 
    
    proj_pump <- read_csv(paste0('data/pumping/projections/',climate_scenario,
                                 '/daily_',pumping_disag,'_',pumping_scenario,'.csv')) %>%
      filter(well == well_sel & radius == pumping_radius) %>%
      select(date, pump = amt) %>%
      filter(date > end_date)
    proj_pump <- rbind(pump, proj_pump)
    write_csv(proj_pump, paste0(input_proj_dir, 'pump.csv'))
  }
}


