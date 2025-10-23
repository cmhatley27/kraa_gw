library(tidyverse)
source('./scripts/functions/Theme+Settings.R')

well_sel <- 'dg01'

climate_radius <- 2
pumping_radius <- 2

year_start <- 1981

use_sel <- 'IRR'

model <- 'ccsm4'
scenario <- 'rcp45'
proj_dir <- paste0('./data/climate/maca/',model,'_',scenario,'/')

#get total annual precip
precip_hist <- read_csv('./data/climate/gridmet/precip.csv') %>%
  filter(radius == climate_radius & well == well_sel) %>%
  group_by(year = year(date)) %>%
  summarise(precip = sum(precip, na.rm = T)) %>%
  filter(year >= year_start)
precip_proj <- read_csv(paste0(proj_dir,'precip.csv')) %>%
  filter(radius == climate_radius & well == well_sel) %>%
  group_by(year = year(date)) %>%
  summarise(precip = sum(precip, na.rm = T)) %>%
  filter(year >= year_start)

eto_hist <- read_csv('./data/climate/gridmet/eto.csv') %>%
  filter(radius == climate_radius & well == well_sel) %>%
  group_by(year = year(date)) %>%
  summarise(eto = sum(eto, na.rm = T)*0.55) %>%
  filter(year >= year_start)
eto_proj <- read_csv(paste0(proj_dir,'eto.csv')) %>%
  filter(radius == climate_radius & well == well_sel) %>%
  group_by(year = year(date)) %>%
  summarise(eto = sum(eto, na.rm = T)*0.55) %>%
  filter(year >= year_start)

#get annual pumping
pumping <- read_csv('./data/pumping/well_annuals.csv') %>%
  filter(radius == pumping_radius & well == well_sel & use == use_sel) %>%
  filter(year >= year_start)

#join precip and eto to pumping and calculate relationship
comb <- left_join(pumping, precip_hist) %>% left_join(., eto_hist) %>%
  mutate(water = precip - eto)

ggplot(comb, aes(x = water, y = amt)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

mod <- lm(amt ~ water, data = comb)
summary(mod)

#use linear model to predict future pumping amounts
projection <- left_join(precip_proj, eto_proj) %>%
  mutate(water = precip-eto)
projection$amt <- predict.lm(mod, projection)
mean(projection$eto)

write_csv(select(projection, year, amt), paste0('./data/pumping/',model,'_',scenario,'/dg01_projection.csv'))



# figure ------------------------------------------------------------------
plot_dat1 <- comb %>%
  mutate(set = 'historical') %>%
  select(year, precip, eto, water, amt, set)
plot_dat2 <- projection %>%
  mutate(set = 'projection')
plot_dat <- rbind(plot_dat1, plot_dat2) %>%
  mutate(amt = amt/1233)

ggplot(subset(plot_dat), aes(x = water, y = amt, color = set)) +
  geom_smooth(data = subset(plot_dat, set == 'historical'), method = 'lm', se = F, linetype = 'dashed') +
  geom_point() +
  labs(x = 'Precip - ET [mm]', y = 'Irrigation Pumping [acre-ft]') +
  scale_color_manual(values = c('black', 'green4'), guide = 'none') +
  xlim(c(-350,1250)) +
  ylim(c(0,350))

