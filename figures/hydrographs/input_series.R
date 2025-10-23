# Load --------------------------------------------------------------------
library(tidyverse)
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
plot_dat <- date_series %>%
  left_join(., riv) %>%
  left_join(., precip_temp) %>%
  left_join(., pump) %>%
  pivot_longer(!dateTime, names_to = 'series', values_to = 'value') %>%
  filter(!is.na(value))
plot_dat$series <- factor(plot_dat$series, levels = c('stage','precip','temp','pumping'),
                          labels = c('River Stage [ft]', 'Precipitation [mm]', 'Temperature [C]', 'Pumping [on/off]'))

ggplot(plot_dat, aes(x = dateTime, y = value)) +
  geom_line() +
  xlab('') +
  ylab('') +
  facet_wrap(vars(series), scales = 'free_y', ncol = 1)


# Save --------------------------------------------------------------------
ggsave(file.path('figures','hydrographs',paste0(well,'_inputs.png')),
       width = 3, height = 4, units = 'in')


# sn01 example ------------------------------------------------------------
well <- read_csv(file.path('data','wells','clean','daily','sn01.csv')) %>%
  select(date, gwl)
clim <- read_csv(file.path('data','mesonet','clean','rossville.csv')) %>%
  select(date, precip, eto = eto_grass)
irr <- read_csv(file.path('data','WIMAS','daily_use','sn01_irr.csv'))

dat <- left_join(well, clim) %>%
  left_join(., irr) %>%
  select(!gwl) %>%
  # convert to imperial units
  mutate(
    precip = precip/25.4,
    eto = eto/25.4,
    irr = irr/43560/(0.3048^3)
  )%>%
  pivot_longer(!date, names_to = 'series', values_to = 'value')
  
dat$series <- factor(dat$series, levels = c('precip','eto','irr'),
                     labels = c('Precipitation [in]', 'Reference ET [in]', 'Irrigation [acre-feet]'))

ggplot(dat, aes(x = date, y = value)) +
  geom_line() +
  xlab('') +
  ylab('') +
  facet_wrap(vars(series), scales = 'free_y', ncol = 1) +
  theme(text = element_text(size = 12))


# Save --------------------------------------------------------------------
ggsave(file.path('figures','hydrographs','sn01_inputs.png'),
       width = 4.5, height = 6, units = 'in') 

# sn01 outputs ------------------------------------------------------------
outputs <- read_csv(file.path('data','modeling_temp','sn01_outputs.csv')) %>%
  select(date = 1, precip = 6, eto = 7, irr = 8) %>%
  pivot_longer(!date, names_to = 'series', values_to = 'value') %>%
  # convert m to ft
  mutate(value = value*3.281)

outputs$series <- factor(outputs$series, levels = c('precip','eto','irr'),
                     labels = c('', ' ', '  '))

ggplot(outputs, aes(x = date, y = value, color = series)) +
  geom_line() +
  xlab('') +
  ylab('') +
  facet_wrap(vars(series), scales = 'free_y', ncol = 1) +
  scale_color_discrete(guide = 'none') +
  theme(text = element_text(size = 12))


# Save --------------------------------------------------------------------
ggsave(file.path('figures','hydrographs','sn01_outputs.png'),
       width = 4.5, height = 6, units = 'in')


# sn01 prediction ---------------------------------------------------------
pred <- read_csv(file.path('data','modeling_temp','sn01_outputs.csv')) %>%
  select(date = 1, observed = 2, predicted = 3) %>%
  mutate(across(!date, ~.x*3.281))
  # pivot_longer(!date, names_to = 'series', values_to = 'value')

ggplot(pred, aes(x = date)) +
  geom_point(aes(y = observed)) +
  geom_line(aes(y = predicted), color = 'steelblue', linewidth = 1) +
  xlab('') +
  ylab('') +
  ggtitle('Groundwater Head [ft]') +
  theme(text = element_text(size = 12))

# Save --------------------------------------------------------------------
ggsave(file.path('figures','hydrographs','sn01_prediction.png'),
       width = 4.5, height = 3, units = 'in')
