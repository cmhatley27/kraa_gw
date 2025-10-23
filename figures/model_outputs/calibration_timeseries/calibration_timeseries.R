# select well and load data -----------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')

well_sel <- 'rl02'
model_name <- 'warmup_mean_center'
metric_sel <- 'kge'

save <- T

model_dir <- paste0('data/models/',well_sel,'/',model_name,'/')
input_dir <- paste0(model_dir,'in/')
output_dir <- paste0(model_dir,'out/')

precip <- read_csv(paste0(input_dir,'precip.csv'))
et <- read_csv(paste0(input_dir,'et.csv'))
pump <- read_csv(paste0(input_dir,'pump.csv'))
gwl <- read_csv(paste0(input_dir,'gwl.csv'))

train_files <- list.files(paste0(output_dir,'train_series/'), full.names = T)
ts <- read_csv(train_files[length(train_files)], col_types = cols(Head_Calibration = 'd')) %>%
  rename(date = 1) %>%
  select(date, sim = Simulation, obs_cal = Head_Calibration, rch = recharge, irr, model_name) %>%
  left_join(gwl) %>%
  rename(obs_all = gwl)

tf_files <- list.files(paste0(output_dir,'transfer_functions/'), full.names = T)
rch_tf <- read_csv(tail(tf_files[str_detect(tf_files, 'rch_pulse')], 1)) %>%
  rename(days = 1)
irr_tf <- read_csv(tail(tf_files[str_detect(tf_files, 'irr_pulse')], 1)) %>%
  rename(days = 1)

perf_files <- list.files(paste0(output_dir,'metrics/'), full.names = T)
train_perf <- lapply(perf_files, read_csv) %>%
  list_rbind(.) %>%
  mutate(metric = tolower(metric),
         set = 'train') %>%
  select(!model_name)

test_files <- list.files(paste0(output_dir,'test_series/'), full.names = T)
test_perf <- lapply(test_files[1:(length(test_files)-1)], read_csv) %>%
  list_rbind(.) %>%
  rename(date = 1) %>%
  select(date, sim = Simulation, model_name, train_set) %>%
  left_join(select(ts, date, obs_cal)) %>%
  group_by(train_set) %>%
  summarise(kge = kge(sim, obs_cal),
            r2 = r2(sim, obs_cal),
            rmse = rmse(sim, obs_cal)) %>%
  pivot_longer(!train_set, names_to = 'metric', values_to = 'value') %>%
  mutate(set = 'test') %>%
  select(metric, value, train_set, set)
perf <- rbind(train_perf, test_perf)

# output timeseries -------------------------------------------------------
ts_rearrange <- arrange(ts, !is.na(obs_cal)) #rearrange output time series so that calibrated points plot on top of uncalibrated points
sim_obs <- ggplot(ts_rearrange, aes(x = date)) +
  geom_point(aes(y = obs_all, color = !is.na(obs_cal))) +
  scale_color_manual(limits = c(F,T), values = c('grey80', 'black'), guide = 'none') +
  geom_line(aes(y = sim), color = 'red') +
  ylab('Groundwater Level [m]') +
  xlab(NULL)
# sim_obs

rch <- ggplot(ts, aes(x = date, y = rch)) +
  geom_line() +
  ylab('irr Contribution [m]') +
  xlab(NULL)
# rch

irr <- ggplot(ts, aes(x = date, y = irr)) +
  geom_line() +
  ylab('Irrigation Contribution [m]') +
  xlab(NULL)
# irr

# transfer functions ------------------------------------------------------
rch_half <- median(rch_tf$days[round(rch_tf$recharge, 4) == round(max(rch_tf$recharge)/2, 4)])
rch_pulse <- ggplot(rch_tf, aes(x = days, y = recharge)) +
  geom_line() +
  annotate('segment', y = 0, yend = rch_tf$recharge[rch_tf$days == rch_half], x = rch_half,
           linetype = 'dashed') +
  xlim(c(0,365*3)) +
  xlab('Days') +
  ylab('Recharge Contribution [m]')
# rch_pulse

rch_integral <- ggplot(rch_tf, aes(x = days, y = cumsum(recharge))) +
 geom_line() +
  annotate('segment', y = 0, yend = cumsum(rch_tf$recharge)[rch_tf$days == rch_half], x = rch_half,
           linetype = 'dashed') +
 xlim(c(0,365*3))+
 xlab('Days') +
 ylab('Recharge Contribution [m]')
# rch_integral


irr_half <- median(irr_tf$days[round(irr_tf$irr, 4) == round(min(irr_tf$irr)/2, 4)])
irr_pulse <- ggplot(irr_tf, aes(x = days, y = irr)) +
  geom_line() +
  annotate('segment', y = 0, yend = irr_tf$irr[irr_tf$days == irr_half], x = irr_half,
           linetype = 'dashed') +
  xlim(c(0,365*3)) +
  xlab('Days') +
  ylab('Irrigation Contribution [m]')
# irr_pulse

irr_integral <- ggplot(irr_tf, aes(x = days, y = cumsum(irr))) +
  geom_line() +
  annotate('segment', y = 0, yend = cumsum(irr_tf$irr)[irr_tf$days == irr_half], x = irr_half,
           linetype = 'dashed') +
  xlim(c(0,365*3))+
  xlab('Days') +
  ylab('Irrigation Contribution [m]')
# irr_integral


# model performance -----------------------------------------------------
perf_plot <- ggplot(subset(perf, metric == metric_sel), 
                    aes(x = value, y = factor(train_set, levels = c(5,4,3,2,1,6)), fill = set)) +
  geom_col(position = position_dodge2(), color = 'black') +
  scale_y_discrete(name = 'Cross-Val. Fold', labels = c('5','4','3','2','1','All')) +
  scale_fill_discrete(name = NULL, 
                    limits = c('train', 'test'), labels = c('Train', 'Test')) +
  scale_x_continuous(name = toupper(metric_sel), limits = c(0,1))
# perf_plot


# combine -----------------------------------------------------------------
library(patchwork)

layout <- '
AAAADD
BBBBEF
CCCCGH
'
sim_obs + rch + irr + perf_plot + rch_pulse + rch_integral + irr_pulse + irr_integral +
  plot_layout(design = layout)
ggsave(paste0('figures/model_outputs/calibration_timeseries/',well_sel,'_',model_name,'.png'),
       height = 7, width = 12.5)
