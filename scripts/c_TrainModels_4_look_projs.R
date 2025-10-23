library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')

well_sel <- 'rl02'
model_name <- 'warmup_step_proj'

model_dir <- paste0('data/models/',well_sel,'/',model_name,'/')
input_dir <- paste0(model_dir,'in/')
output_dir <- paste0(model_dir,'out/')

obs <- read_csv(paste0(input_dir,'gwl.csv'))

proj_files <- list.files(paste0(output_dir,'projections/'), full.names = T)

proj <- lapply(proj_files, read_csv) %>%
  list_rbind(.) %>%
  select(date, obs = Head_Calibration, sim = Simulation, rch = recharge, irr, scenario)
ggplot(proj, aes(x = date, y = sim, color = scenario)) +
  geom_line()

proj_long <- proj %>%
  select(!obs) %>%
  pivot_longer(!c(date, scenario), names_to = 'stress', values_to = 'val') %>%
  mutate(stress = factor(stress, levels = c('sim', 'rch', 'irr')))
ggplot(proj_long, aes(x = date, y = val, color = scenario)) +
  geom_line() +
  facet_wrap(vars(stress), ncol = 1, scales = 'free_y')

proj_annual <- proj %>%
  group_by(year = year(date), scenario) %>%
  summarise(across(c(sim, rch, irr), mean))
ggplot(proj_annual, aes(x = year, y = sim, color = scenario)) +
  geom_line()

proj_annual_long <- proj_annual %>%
  pivot_longer(!c(year, scenario), names_to = 'stress', values_to = 'val') %>%
  mutate(stress = factor(stress, levels = c('sim', 'rch', 'irr')))
ggplot(subset(proj_annual_long, year >= 1990), aes(x = year, y = val, color = scenario)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(vars(stress), ncol = 1, scales = 'free_y')


asdf <- mutate(proj_annual, period = cut(year, c(0,2024,2099), labels = c('hist', 'proj')))
ggplot(subset(asdf, year >= 1990), aes(x = rch, y = irr, color = period)) +
  geom_point() +
  facet_wrap(vars(scenario))
