library(tidyverse)
source('scripts/functions/theme.R')

hist_pet <- read_csv('data/climate/gridmet/pet.csv') %>%
  filter(radius == 2) %>%
  group_by(year = year(date), well) %>%
  summarize(eto = sum(pet, na.rm = T)) %>%
  mutate(source = 'hist_pet')
hist_eto <- read_csv('data/climate/gridmet/eto.csv') %>%
  filter(radius == 2) %>%
  group_by(year = year(date), well) %>%
  summarize(eto = sum(eto, na.rm = T)) %>%
  mutate(source = 'hist')
proj_eto <- read_csv('data/climate/maca/ccsm4_rcp45/eto.csv') %>%
  filter(radius == 2) %>%
  group_by(year = year(date), well) %>%
  summarize(eto = sum(eto, na.rm = T)) %>%
  mutate(source = 'proj')
dat <- rbind(hist_eto, proj_eto, hist_pet) %>%
  mutate(period = cut(year, c(0,2025,2050,2075,2100), labels = c('hist', '1','2','3'), right = F))

ggplot(subset(dat, well == 'dg01' & year >= 2006), aes(x = period, y = eto, fill = source)) +
  geom_boxplot()
