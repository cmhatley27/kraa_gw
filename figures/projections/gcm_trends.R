# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')

climate_radius <- 2 #climate values are not sensitive to the radius since they are already area normalized [mm]

projs <- list.files('data/climate/maca/')
projs <- projs[!str_detect(projs, 'elev')]

proj_month <- tibble()
for(i in 1:length(projs)){
  proj <- projs[i]
  precip_i <- read_csv(paste0('data/climate/maca/',proj,'/precip.csv')) %>%
    mutate(gcm = proj) %>%
    filter(radius == climate_radius) %>%
    select(gcm, date, well, precip) %>%
    group_by(gcm, year = year(date), month = month(date), well) %>%
    summarise(precip = sum(precip, na.rm = T))
  eto_i <- read_csv(paste0('data/climate/maca/',proj,'/eto.csv')) %>%
    mutate(gcm = proj) %>%
    filter(radius == climate_radius) %>%
    select(gcm, date, well, eto) %>%
    group_by(gcm, year = year(date), month = month(date), well) %>%
    summarise(eto = sum(eto, na.rm = T))
  eta_i <- read_csv(paste0('data/climate/maca/',proj,'/eta_kc.csv')) %>%
    mutate(gcm = proj) %>%
    filter(radius == climate_radius) %>%
    select(gcm, date, well, eta) %>%
    group_by(gcm, year = year(date), month = month(date), well) %>%
    summarise(eta = sum(eta, na.rm = T))
  climate_i <- left_join(precip_i, eto_i) %>% left_join(., eta_i)
  
  proj_month <- rbind(proj_month, climate_i)
  rm('precip_i', 'eto_i', 'eta_i', 'climate_i')
}
proj_annual <- proj_month %>%
  group_by(gcm, year, well) %>%
  summarise(across(c(precip, eto, eta), sum)) %>%
  pivot_longer(c(precip, eto, eta), names_to = 'var', values_to = 'val') %>%
  filter(year >= 2000)

hist_precip <- read_csv('data/climate/gridmet/precip.csv') %>%
  filter(radius == climate_radius) %>%
  group_by(year = year(date), month = month(date), well) %>%
  summarise(precip = sum(precip))
hist_eto <- read_csv('data/climate/gridmet/eto.csv') %>%
  filter(radius == climate_radius) %>%
  group_by(year = year(date), month = month(date), well) %>%
  summarise(eto = sum(eto))
hist_eta <- read_csv('data/climate/gridmet/eta_kc.csv') %>%
  filter(radius == climate_radius) %>%
  group_by(year = year(date), month = month(date), well) %>%
  summarise(eta = sum(eta))
hist_month <- left_join(hist_precip, hist_eto) %>% left_join(., hist_eta) %>%
  mutate(gcm = 'hist', .before = year)
hist_annual <- hist_month %>%
  group_by(gcm, year, well) %>%
  summarise(across(c(precip, eto, eta), sum)) %>%
  pivot_longer(c(precip, eto, eta), names_to = 'var', values_to = 'val') %>%
  filter(year %in% 1981:2024)

# box plot ----------------------------------------------------------------
well_sel <- c('dg01', 'rl02')

dat_annual <- rbind(hist_annual, proj_annual) %>%
  mutate(period = cut(year, c(0,2025,2050,2075,2100), labels = c('hist', '1','2','3'), right = F))

ggplot(subset(dat_annual, well %in% well_sel), aes(x = period, y = val, fill = gcm)) +
  geom_boxplot() +
  facet_grid(rows = vars(var), cols = vars(well), scales = 'free_y')

ggplot(subset(dat_annual, well %in% well_sel & var == 'precip'), aes(x = year, y = val, color = gcm)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(vars(well))


