# select well and load data -----------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')

well_sel <- 'rl02'
model_name <- 'warmup_step'
year_sel <- 2019:2020

save <- T

model_dir <- paste0('data/models/',well_sel,'/',model_name,'/')
input_dir <- paste0(model_dir,'in/')
output_dir <- paste0(model_dir,'out/')
settings <- read_csv(paste0(model_dir, 'input_settings.csv'))
recharge_tf <- settings$value[settings$setting == 'rch_function']

precip <- read_csv(paste0(input_dir,'precip.csv'))
et <- read_csv(paste0(input_dir,'et.csv'))
gwl <- read_csv(paste0(input_dir,'gwl.csv'))

param_files <- list.files(paste0(output_dir,'params/'), full.names = T)
params <- read_csv(param_files[length(param_files)]) %>%
  rename(param = 1)


# gwl plot ----------------------------------------------------------------
gwl_plot <- ggplot(subset(gwl, year(date) %in% year_sel & !is.na(gwl)), aes(x = date, y = gwl)) +
  geom_line() +
  scale_x_date(name = NULL) +
  ylab('Groundwater Level')
# gwl_plot

# recharge model ----------------------------------------------------------
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

gam = params$optimal[params$param == 'recharge_gamma']
ks = params$optimal[params$param == 'recharge_ks']
srmax = params$optimal[params$param == 'recharge_srmax']

model_output <- smodel(precip$date, precip$precip, et$et, gam, ks, srmax) %>%
  left_join(gwl)

annual_rch <- group_by(model_output, year = year(date)) %>%
  summarise(r = sum(r))
print(paste0('Mean Annual Recharge: ',mean(annual_rch$r)))
print(paste0('Selected Annual Recharge: ',annual_rch$r[annual_rch$year %in% year_sel]))

sm_plot <- ggplot(subset(model_output, year(date) == year_sel),
                  aes(x = date, y = s/srmax)) +
  geom_hline(yintercept = 0) +
  geom_line(color = 'red') +
  geom_col(aes(y = p/max(p)), alpha = 0.2) +
  ylim(c(0,1.15)) +
  scale_x_date(name = NULL) +
  ylab('Soil Moisture [%]')
# sm_plot

sm_rch_relationship <- ggplot(subset(model_output, year(date) == year_sel),
                              aes(y = s/srmax, x = ks*(s/srmax)^gam)) +
  geom_point(color = 'black') +
  scale_y_continuous(name = 'Soil Moisture [%]', limits = c(0,1.15)) +
  xlab('Recharge [mm]')
# sm_rch_relationship

rch_plot <- ggplot(subset(model_output, year(date) == year_sel),
                   aes(x = date, y = r)) +
  geom_hline(yintercept = 0) +
  geom_line(color = 'blue') +
  geom_col(aes(y = p), alpha = 0.2) +
  scale_x_date(name = NULL) +
  ylab('Recharge [mm]')
# rch_plot


# convolutions ------------------------------------------------------------
ef <- function(x,A,a) (A/a)*(exp(-x/a))
gamf <- function(x,A,n,a) A*(x^(n-1))*exp(-x/a)/((a^n)*gamma(n))

rch_A = params$optimal[params$param == 'recharge_A']
rch_a = params$optimal[params$param == 'recharge_a']
rch_n = params$optimal[params$param == 'recharge_n']

if(recharge_tf == 'exponential') rch_tf <- ef(seq(1, nrow(model_output)), rch_A, rch_a)
if(recharge_tf == 'gamma') rch_tf <- gamf(seq(1, nrow(model_output)), rch_A, rch_n, rch_a)

rch_conv <- convolve(model_output$r, rev(rch_tf), type = 'open')[1:length(rch_tf)]

tf_plot <- tibble(x = seq(1,length(rch_tf)), y = rch_tf) %>%
  ggplot(., aes(x,y)) +
    geom_line() +
    scale_x_continuous(name = 'Days', limits = c(0, 365*5)) +
    ylab('GW Rise [m]')
# tf_plot

conv_plot <- tibble(date = precip$date, r = rch_conv) %>%
  filter(year(date) %in% year_sel) %>%
  ggplot(., aes(x = date, y = r)) +
    geom_line() +
    scale_x_date(name = NULL) +
    ylab('GW Rise [m]')
# conv_plot

# combine -----------------------------------------------------------------
library(patchwork)

(sm_plot + sm_rch_relationship + rch_plot) /
(tf_plot + conv_plot + gwl_plot)

if(save == T){
ggsave(paste0('figures/model_outputs/recharge_details/',well_sel,'_',model_name,'.png'),
       height = 6, width = 9)
}
