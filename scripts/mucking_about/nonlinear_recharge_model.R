library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')


# select model outputs ----------------------------------------------------
#look at how model parameters for recharge model can vary (srmax, ks, gamma)

well_sel <- 'rl02'
model_name <- c('flex_exp')

model_dir <- paste0('data/models/',well_sel,'/',model_name,'/')
input_dir <- paste0(model_dir,'in/')
output_dir <- paste0(model_dir,'out/')
output_metrics_dir <- paste0(output_dir,'metrics/')
output_params_dir <- paste0(output_dir,'params/')
output_train_dir <- paste0(output_dir,'train_series/')
output_test_dir <- paste0(output_dir,'test_series/')

precip <- read_csv(paste0(input_dir[1],'precip.csv'))
et <- read_csv(paste0(input_dir[1],'et.csv'))
gwl <- read_csv(paste0(input_dir[1],'gwl.csv'))

param_files <- list.files(output_params_dir, full.names = T)
params <- lapply(param_files, read_csv, col_types = cols(model_name = 'c')) %>%
  list_rbind(.) %>%
  rename(param = 1)

ggplot(params, aes(x = train_set, color = model_name, y = optimal)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(param), scales = 'free_y')

# simulate soil box -------------------------------------------------------
smodel <- function(date, p, e, gam, ks, srmax){
  s0 = double(length(date))
  r = double(length(date))
  s = double(length(date))
  
  for(i in 1:length(date)){
    if(i == 1) s0[i] = srmax*0.5 else s0[i] = s[i-1]
    r[i] = ks*((s0[i]/srmax)^gam)
    s[i] = max((s0[i] + p[i] - e[i] - r[i]), 0) 
  }
  out <- tibble(date, s0, p, e, r, s) %>% mutate(r_lin = p-e)
  return(out)
}
# model_test <- smodel(precip$date, precip$precip, et$et, 2, 200, 250)
# 
# ggplot(subset(model_test, year(date) == 2016), aes(x = date)) +
#   geom_col(aes(y = p)) +
#   geom_line(aes(y = s)) +
#   geom_line(aes(y = -e), color = 'red') +
#   geom_line(aes(y = -r), color = 'blue') 
# 
# ggplot(subset(model_test, year(date) == 2016), aes(x = date)) +
#   geom_hline(yintercept = 0) +
#   geom_line(aes(y = r_lin), color = 'red') +
#   geom_line(aes(y = r), color = 'blue')
# 
# ggplot(model_test, aes(x = lag(r_lin), y = r)) +
#   geom_point() +
#   geom_abline(slope = 1)


# bring in model parameter sets -------------------------------------------
param_sets <- select(params, model_name, train_set, param, optimal) %>%
  filter(param %in% c('recharge_srmax', 'recharge_ks', 'recharge_gamma')) %>%
  pivot_wider(id_cols = c(model_name, train_set), names_from = 'param', values_from = 'optimal')

model_output <- data.frame()
for(run in 1:nrow(param_sets)){
  model_name = param_sets$model_name[run]
  train_set = param_sets$train_set[run]
  gam = param_sets$recharge_gamma[run]
  ks = param_sets$recharge_ks[run]
  srmax = param_sets$recharge_srmax[run]
  
  run_out <- smodel(precip$date, precip$precip, et$et, gam, ks, srmax) %>%
    mutate(model_name = model_name, train_set = train_set)
  model_output = rbind(model_output, run_out)
}
model_output <- left_join(model_output, gwl)


annual_rch <- model_output %>% group_by(model_name, train_set, year = year(date)) %>%
  summarise(r = sum(r)) 
annual_rch %>% group_by(model_name, train_set) %>% summarise(r = mean(r))

year_sel <- 2019

ggplot(subset(model_output, year(date) == year_sel & train_set == 6 & model_name == 'flex_exp'),
       aes(x = date, y = s/param_sets$recharge_srmax[6])) +
  geom_hline(yintercept = 0) +
  geom_line(color = 'brown') +
  geom_col(aes(y = p/max(p)), alpha = 0.2) +
  ylim(c(0,1.15)) +
  scale_x_date(name = NULL, labels = NULL) +
  ylab('Soil Moisture')

ggplot(subset(model_output, year(date) == year_sel & train_set == 6 & model_name == 'flex_exp'),
       aes(x = date, y = r)) +
  geom_hline(yintercept = 0) +
  geom_col(aes(x = date, y = p), alpha = 0.2) +
  geom_line(color = 'blue') +
  scale_x_date(name = NULL, labels = NULL) +
  ylim(c(0,100)) +
  ylab('Recharge [mm]')

ggplot(subset(model_output, year(date) == year_sel & train_set == 6), aes(
  x = s/(param_sets$recharge_srmax[6]),
  y = param_sets$recharge_ks[6]*((s/(param_sets$recharge_srmax[6]))^param_sets$recharge_gamma[6])
)) +
  geom_point(color = 'blue') +
  scale_x_continuous(name = 'Soil Moisture', limits = c(0,1.15)) +
  ylab('Recharge')

# convolve with transfer function -----------------------------------------
#exponential function
ef <- function(x,A,a) (A/a)*(exp(-x/a))
gamf <- function(x,A,n,a) A*(x^(n-1))*exp(-x/a)/((a^n)*gamma(n))

rch_A = params$optimal[params$param == 'recharge_A' & params$train_set == 6]
rch_a = params$optimal[params$param == 'recharge_a' & params$train_set == 6]
rch_n = params$optimal[params$param == 'recharge_n' & params$train_set == 6]

lin_rch <- model_output$r_lin[model_output$train_set == 6]
mod_rch <- model_output$r[model_output$train_set == 6]

rch_gamf <- gamf(seq(1:length(mod_rch)), rch_A, rch_n, rch_a)
rch_ef <- ef(seq(1:length(mod_rch)), rch_A, rch_a)

rch_conv <- convolve(mod_rch, rev(rch_ef), type = 'open')[1:length(mod_rch)]

# plot(mod_rch, type = 'l')
# plot(rch_conv, type = 'l')

transfer_plot <- data.frame(
  x = 1:(365*4),
  y = rch_ef[1:(365*4)]
)
ggplot(transfer_plot, aes(x, y)) +
  geom_line() +
  ylab('GW Rise') +
  xlab('Days')

conv_plot <- data.frame(y = rch_conv[which(year(model_output$date) == year_sel & model_output$train_set == 1)])
conv_plot$x = seq(1,length(conv_plot$y))
  
ggplot(conv_plot, aes(x,y)) +
  geom_line() +
  scale_x_continuous(labels = NULL, name = NULL) +
  ylab('GW Rise')

ggplot(subset(model_output, year(date) == year_sel & train_set == 6 & model_name == 'flex_exp'),
       aes(x = date, y = gwl)) +
  geom_line() +
  scale_x_date(name = NULL, labels = NULL) +
  ylab('Groundwater Level')
