# select model runs -------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')
source('scripts/functions/performance_metrics.R')

well_sel <- 'rl02'
model_name <- c('base', 'warmup', 'mean_center', 'step', 'warmup_mean_center', 'warmup_step')

model_dir <- paste0('data/models/',well_sel,'/',model_name,'/')
input_dir <- paste0(model_dir,'in/')
output_dir <- paste0(model_dir,'out/')
output_metrics_dir <- paste0(output_dir,'metrics/')
output_params_dir <- paste0(output_dir,'params/')
output_train_dir <- paste0(output_dir,'train_series/')
output_test_dir <- paste0(output_dir,'test_series/')
output_tf_dir <- paste0(output_dir,'transfer_functions/')

eval_periods <- nrow(read_csv(paste0(model_dir[1],'eval_settings.csv')))-1
full_cal <- eval_periods+1

# compare ts --------------------------------------------------------------
obs <- tibble()
for(i in 1:length(model_name)){
  obs_i <- read_csv(paste0(input_dir[i],'gwl.csv')) %>% rename(obs = gwl) %>%
    mutate(model_name = model_name[i])
  obs <- rbind(obs,obs_i)
}

train_files <- list.files(output_train_dir, full.names = T)
train <- lapply(train_files, read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(date = 1) %>%
  select(date, sim = Simulation, model_name, train_set) %>%
  mutate(period = 'train')

# ggplot(train, aes(x = date, y = sim, color = factor(train_set))) +
#   geom_point(data = obs, aes(x = date, y = gwl), inherit.aes = F) +
#   geom_line() +
#   facet_wrap(vars(train_set))

test_files <- list.files(output_test_dir, full.names = T)
test <- lapply(test_files, read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(date = 1) %>%
  select(date, sim = Simulation, model_name, train_set) %>%
  mutate(period = 'test')

# ggplot(test, aes(x = date, y = sim, color = factor(train_set))) +
#   geom_point(data = subset(obs, date >= min(test$date)), aes(x = date, y = gwl), inherit.aes = F) +
#   geom_line() +
#   facet_wrap(vars(train_set))

ts <- rbind(train, test) %>%
  mutate(period = factor(period, levels = c('train', 'test'))) %>%
  left_join(obs)

ggplot(subset(ts, train_set != full_cal), aes(x = date, y = sim, color = period)) +
  geom_point(data = subset(ts, train_set != full_cal), aes(x = date, y = obs), inherit.aes = F) +
  geom_line() +
  facet_wrap(vars(model_name, train_set), ncol = eval_periods, scales = 'free_y') +
  xlab(NULL) +
  ylab('gwl [m]') +
  scale_color_manual(limits = c('train', 'test'), values = c('#F8766D', '#619CFF')) +
  theme(legend.position = 'bottom')

# compare fit metrics -----------------------------------------------------
bm <- ts %>%
  select(!sim) %>%
  group_by(model_name, train_set, period) %>%
  mutate(sim = mean(obs, na.rm = T)) %>%
  filter(period == 'test') %>%
  mutate(period = 'bm') %>%
  select(date, sim, model_name, train_set, period, obs)
ts_bm <- rbind(ts, bm)
  
metrics <- ts_bm %>%
  group_by(model_name, train_set, period) %>%
  filter(!is.na(obs)) %>%
  summarise(r2 = r2(sim, obs),
            kge = kge(sim, obs),
            rmse = rmse(sim, obs)) %>%
  pivot_longer(c(r2, kge, rmse), names_to = 'metric', values_to = 'value') %>%
  filter(!(metric %in% c('kge', 'r2') & period == 'bm'))

metric_sel = 'rmse'
ggplot(subset(metrics, metric == metric_sel & train_set != full_cal), 
       aes(x = period, color = factor(train_set), y = value)) + 
  # geom_hline(data = subset(metrics, metric == metric_sel & train_set == full_cal),
  #            aes(yintercept = value), linetype = 'dashed', alpha = 0.5) +
  geom_point() + 
  facet_wrap(vars(model_name)) +
  ylab(metric_sel) +
  ylim(c(0,1)) +
  scale_color_discrete(name = 'Fold')

metrics_summary <- metrics %>%
  filter(train_set != full_cal) %>%
  group_by(model_name, period, metric) %>%
  summarise(mean = mean(value),
            median = median(value))

ggplot(subset(metrics_summary, metric == metric_sel), aes(x = period, fill = period, y = mean)) +
  geom_col(color = 'black') +
  geom_text(aes(x = period, label = round(mean, 2), y = mean), nudge_y = -0.05) +
  facet_grid(cols = vars(metric), rows = vars(model_name)) +
  scale_fill_manual(limits = c('train', 'test', 'bm'), values = c('#F8766D', '#619CFF', '#00BA38'),
                    guide = 'none')

# compare parameters ------------------------------------------------------
param_files <- list.files(output_params_dir, full.names = T)
params <- lapply(param_files[-seq(full_cal, length(param_files), by = full_cal)], read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(param = 1)
full_cal_params <- read_csv(param_files[seq(full_cal, length(param_files), by = full_cal)], col_types = cols(model_name = 'c')) %>%
  rename(param = 1)

ggplot(params, aes(x = train_set, color = model_name, y = optimal)) +
  geom_hline(data = full_cal_params, aes(yintercept = optimal, color = model_name), linetype = 'dashed', alpha = 0.7) +
  geom_line() +
  # geom_errorbar(aes(ymin = optimal - 2*stderr, ymax = optimal + 2*stderr)) +
  geom_point() +
  facet_wrap(vars(param), scales = 'free_y')

param_summary <- group_by(params, model_name, param) %>%
  summarise(val = mean(optimal))


# look at transfer functions ----------------------------------------------
tf_files <- list.files(output_tf_dir, full.names = T)
rch_pulses <- lapply(tf_files[str_detect(tf_files, 'rch_pulse')], read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(days = 1, val = 2) %>%
  mutate(stress = 'rch')
rch_integrals <- lapply(tf_files[str_detect(tf_files, 'rch_integral')], read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(days = 1, val = 2) %>%
  mutate(stress = 'rch')
irr_pulses <- lapply(tf_files[str_detect(tf_files, 'irr_pulse')], read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(days = 1, val = 2) %>%
  mutate(stress = 'irr')
irr_integrals <- lapply(tf_files[str_detect(tf_files, 'irr_integral')], read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(days = 1, val = 2) %>%
  mutate(stress = 'irr')

#rch pulse function
ggplot(rch_pulses, aes(x = days, y = val, color = factor(train_set))) +
  geom_vline(xintercept = seq(365,365*3,by = 365), linetype = 'dashed', alpha = 0.5) +
  geom_line() +
  facet_wrap(vars(model_name)) +
  scale_color_discrete(name = 'Fold') +
  ylab('Rch')
ggplot(rch_pulses, aes(x = days, y = val, color = model_name, asdf = factor(train_set))) +
  geom_line() +
  facet_wrap(vars(factor(train_set)))

#rch integrals
ggplot(rch_integrals, aes(x = days, y = val, color = factor(train_set))) +
  geom_vline(xintercept = seq(365,365*3,by = 365), linetype = 'dashed', alpha = 0.5) +
  geom_line() +
  facet_wrap(vars(model_name))
ggplot(rch_integrals, aes(x = days, y = val, color = model_name, asdf = factor(train_set))) +
  geom_vline(xintercept = seq(365,365*3,by = 365), linetype = 'dashed', alpha = 0.5) +
  geom_line() +
  facet_wrap(vars(factor(train_set)))

#irr pulse function
ggplot(irr_pulses, aes(x = days, y = val, color = factor(train_set))) +
  geom_vline(xintercept = seq(365,365*3,by = 365), linetype = 'dashed', alpha = 0.5) +
  geom_line() +
  facet_wrap(vars(model_name)) +
  scale_color_discrete(name = 'Fold') +
  ylab('Irr')
ggplot(irr_pulses, aes(x = days, y = val, color = model_name, asdf = factor(train_set))) +
  geom_vline(xintercept = seq(365,365*3,by = 365), linetype = 'dashed', alpha = 0.5) +
  geom_line() +
  facet_wrap(vars(factor(train_set)))

ggplot(irr_integrals, aes(x = days, y = val, color = factor(train_set))) +
  geom_vline(xintercept = seq(365,365*3,by = 365), linetype = 'dashed', alpha = 0.5) +
  geom_line() +
  facet_wrap(vars(model_name))
ggplot(irr_integrals, aes(x = days, y = val, color = model_name, asdf = factor(train_set))) +
  geom_line() +
  facet_wrap(vars(factor(train_set)))

# full calibration stress time series -------------------------------------
train_files <- list.files(output_train_dir, full.names = T)
ts <- lapply(train_files, read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(date = 1) %>%
  filter(train_set == full_cal) %>%
  select(date, sim = Simulation, obs = Head_Calibration, rch = recharge, irr, model_name)

ts_long <- pivot_longer(ts, c(sim, rch, irr), names_to = 'stress', values_to = 'val') %>%
  mutate(stress = factor(stress, levels = c('sim', 'rch', 'irr')))
ggplot(subset(ts_long, model_name %in% c('base', 'step', 'warmup_step')), aes(x = date, y = val, color = model_name)) +
  geom_line() +
  facet_wrap(vars(stress), scales = 'free_y', ncol = 1)

ts_rch_wide <- pivot_wider(select(ts, date, model_name, rch), id_cols = date, names_from = model_name, values_from = rch)
ggplot(subset(ts_rch_wide, year(date) %in% 2019), aes(x = step, y = warmup_step, color = date)) +
  geom_abline(slope=1) +
  geom_point()

annual_contributions <- ts_long %>%
  mutate(wateryear = ifelse(month(date) >= 10, year(date)+1, year(date))) %>%
  filter(stress != 'sim') %>%
  group_by(model_name, wateryear, stress) %>%
  summarise(sum = sum(val, na.rm = T),
            mean = mean(val, na.rm = T)) %>%
  mutate(across(c(sum, mean), ~abs(.x)))
ggplot(annual_contributions, aes(x = model_name, fill = stress, y = mean)) +
  geom_boxplot()

# stage transfer functions ------------------------------------------------
stage_pulses <- lapply(tf_files[str_detect(tf_files, 'stage_pulse')], read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(days = 1, val = 2) %>%
  mutate(stress = 'stage')
stage_integrals <- lapply(tf_files[str_detect(tf_files, 'stage_integral')], read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(days = 1, val = 2) %>%
  mutate(stress = 'stage')


#stage pulse function
ggplot(stage_pulses, aes(x = days, y = val, color = factor(train_set))) +
  # geom_vline(xintercept = seq(365,365*3,by = 365), linetype = 'dashed', alpha = 0.5) +
  geom_line() +
  facet_wrap(vars(model_name)) +
  scale_color_discrete(name = 'Fold') +
  ylab('stage')
ggplot(stage_pulses, aes(x = days, y = val, color = model_name, asdf = factor(train_set))) +
  geom_line() +
  facet_wrap(vars(factor(train_set)))

ggplot(stage_integrals, aes(x = days, y = val, color = factor(train_set))) +
  geom_line() +
  facet_wrap(vars(model_name))
ggplot(stage_integrals, aes(x = days, y = val, color = model_name, asdf = factor(train_set))) +
  geom_line() +
  facet_wrap(vars(factor(train_set)))

#stage vs recharge
rch_stage_comp <- rbind(rch_pulses, stage_pulses) %>%
  filter(train_set == full_cal)
ggplot(rch_stage_comp, aes(x = days, y = val, color = model_name, linetype = stress)) +
  geom_line() +
  ylim(c(0,0.01)) +
  xlim(c(0,3000))
  

# full calibration stress time series WITH STAGE -------------------------------------
train_files <- list.files(output_train_dir, full.names = T)
ts <- lapply(train_files, read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(date = 1) %>%
  filter(train_set == full_cal) %>%
  select(date, sim = Simulation, obs = Head_Calibration, rch = recharge, irr, stage, model_name) %>%
  mutate(rch_plus_stage = rch + stage)

ts_long <- pivot_longer(ts, c(sim, rch, irr, stage), names_to = 'stress', values_to = 'val') %>%
  mutate(stress = factor(stress, levels = c('sim', 'rch', 'stage', 'irr')))
ggplot(ts_long, aes(x = date, y = val, color = model_name)) +
  geom_line() +
  facet_wrap(vars(stress), scales = 'free_y', ncol = 1)

ts_rch_wide <- pivot_wider(select(ts, date, model_name, rch), id_cols = date, names_from = model_name, values_from = rch)
ggplot(subset(ts_rch_wide, year(date) %in% 2019), aes(x = flex_exp, y = river_gam, color = date)) +
  geom_abline(slope=1) +
  geom_point()

ggplot(subset(ts, model_name == 'river_gam' & year(date) %in% 2017:2020), aes(x = rch, y = stage, color = date)) +
  geom_path() +
  scale_color_viridis_c()
