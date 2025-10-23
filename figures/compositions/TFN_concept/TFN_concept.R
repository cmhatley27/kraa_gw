# Load --------------------------------------------------------------------
library(tidyverse)
library(zoo)
source(file.path('scripts','functions','Theme+Settings.R'))

wells <- map(list.files(file.path('data','wells','clean'), full.names = T), read_csv) %>%
  list_rbind()


# Select well and input series --------------------------------------------
well <- 'lv01'
river_gage <- 'Desoto'
precip_gage <- 'Lawrence_Airport'

#load well for dateTimes
date_series <- wells %>%
  filter(id == well) %>%
  select(dateTime)

#load river
riv <- read_csv(file.path('data','river','clean',paste0(river_gage,'.csv'))) %>%
  select(!id)

#load precip and temp
precip_temp <- read_csv(file.path('data','precip',paste0(precip_gage,'.csv'))) %>%
  filter(year(DATE) >= 2018) %>%
  mutate(temp = ((TMAX + TMIN)/2)/10) %>%
  mutate(temp = na.approx(temp)) %>%
  select(dateTime = DATE, precip = PRCP, temp) %>%
  mutate(precip = ifelse(is.na(precip), 0 , precip),
         precip = precip/10)
#arbitrarily set precip to noon
hour(precip_temp$dateTime) <- 12

#generate pumping
pump <- tibble(dateTime = date_series$dateTime) %>%
  mutate(pumping = ifelse(month(dateTime) %in% c(5:8),1,0))

#combine
dat <- date_series %>%
  left_join(., riv) %>%
  left_join(., precip_temp) %>%
  left_join(., pump)
plot_dat <- dat %>%
  pivot_longer(!dateTime, names_to = 'series', values_to = 'value') %>%
  filter(!is.na(value))
plot_dat$series <- factor(plot_dat$series, levels = c('stage','precip','temp','pumping'),
                          labels = c('River Stage [ft]', 'Precipitation [mm]', 'Temperature [C]', 'Pumping [on/off]'))

ggplot(plot_dat, aes(x = dateTime, y = value)) +
  geom_line() +
  xlab('') +
  ylab('') +
  facet_wrap(vars(series), scales = 'free_y', ncol = 1)


# Save inputs -------------------------------------------------------------
ggsave(file.path('figures','compositions','TFN_concept',paste0(well,'_inputs.png')),
       width = 2.5, height = 4, units = 'in')

# Set Test Transfer Functions ------------------------------------------------
p3 <- function(x, A, a, n){
  theta <- A*((a^n)*(x^(n-1))*(exp(-1*a*x)))/gamma(n)
  return(theta)
}

x <- seq(0,100, by = 0.01)
p3_test <- tibble(
  x = x,
  riv_transform = p3(x,0.01,0.2,1.1),
  p_transform = p3(x,0.2,0.1,5),
  pet_transform = p3(x,0.2,0.1,5)*-1*0.7,
  pump_transform = p3(x,0.05,0.2,4)
) %>%
  pivot_longer(!x, names_to = 'params', values_to = 'y')
p3_test$params <- factor(p3_test$params, 
                         levels = c('riv_transform','p_transform','pet_transform','pump_transform'), 
                         labels = c('',' ','  ','   '))

ggplot(p3_test) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_line(aes(x = x, y = y, color = params), linewidth = 1) +
  facet_wrap(vars(params), ncol = 1) +
  ylab('') +
  xlab('Days') +
  scale_color_discrete(guide = 'none') +
  theme(axis.text.y = element_blank())


# Save Transfer Functions -------------------------------------------------
ggsave(file.path('figures','compositions','TFN_concept','transfer_functions.png'),
       width = 1.5, height = 4, units = 'in')

# Run response function on input series -----------------------------------
p3_length <- 100

riv.c <- riv %>%
  mutate(stage.c = convolve(stage, p3(seq(0:p3_length),0.05,0.2,1.1), type = 'o', conj = F)[-(1:p3_length)]) %>%
  select(!stage)

precip_temp.c <- precip_temp %>%
  mutate(
    precip.c = convolve(precip, p3(seq(0:p3_length),0.8,0.1,5), type = 'o', conj = F)[-(1:p3_length)],
    temp.c = -1*0.1*convolve(temp, p3(seq(0:p3_length),0.2,0.1,5), type = 'o', conj = F)[-(1:p3_length)]
  ) %>%
  select(!c(precip, temp))

pump.c <- pump %>%
  mutate(pumping.c = convolve(pumping, p3(seq(0:p3_length),0.05,0.2,4), type = 'o', conj = F)[-(1:p3_length)]) %>%
  select(!pumping)

dat.c <- date_series %>%
  left_join(., riv.c) %>%
  left_join(., precip_temp.c) %>%
  left_join(., pump.c)

plot_dat.c <- dat.c %>%
  pivot_longer(!dateTime, names_to = 'series', values_to = 'value') %>%
  filter(!is.na(value))
plot_dat.c$series <- factor(plot_dat.c$series, levels = c('stage.c','precip.c','temp.c','pumping.c'),
                          labels = c('Stage Response', 'Precip Response', 'Temp Response', 'Pumping Response'))

ggplot(plot_dat.c, aes(x = dateTime, y = value, color = series)) +
  geom_line() +
  xlab('') +
  ylab('') +
  facet_wrap(vars(series), scales = 'free_y', ncol = 1) +
  scale_color_discrete(guide = 'none') 

# Save Responses ----------------------------------------------------------
ggsave(file.path('figures','compositions','TFN_concept','lv01_responses.png'),
       width = 2.5, height = 4, units = 'in')


# Add transformed inputs --------------------------------------------------
trim_value <- 750

well_dat <- wells %>%
  filter(id == well & gwl >= trim_value) %>%
  select(!id)

trimmed_dates <- dat.c %>%
  filter(!is.na(precip.c))  %>%
  select(dateTime)

transform_sum <- dat.c %>%
  filter(!is.na(precip.c)) %>%
  select(!dateTime) %>%
  rowSums()

dat_sum <- tibble(
  dateTime = trimmed_dates$dateTime,
  pred = transform_sum
) %>%
  left_join(., well_dat) %>%
  mutate(pred = pred + (mean(gwl) - mean(pred, na.rm = T))) %>%
  pivot_longer(!dateTime, names_to = 'series', values_to = 'val')

ggplot(dat_sum, aes(x = dateTime, y = val, color = series)) +
  geom_line() +
  ylab('Well Head [ft]') +
  xlab('') +
  scale_color_manual(guide = 'none', values = c('black', 'red'))

# Save Final --------------------------------------------------------------
ggsave(file.path('figures','compositions','TFN_concept','hydrograph_comparison.png'),
       width = 6.5, height = 2, units = 'in')





  
  
  

