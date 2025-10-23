library(tidyverse)

# establish historical irrigation relationships ---------------------------
hist_pumping <- read_csv('data/pumping/wimas/annual.csv') %>%
  filter(use == 'IRR', year >= 1981)

#Select the climate radius for establishing irrigation-climate relationships.
#Annual water balances are not sensitive to this radius so the choice doesn't
#really matter.
climate_radius <- 2

hist_precip <- read_csv('data/climate/gridmet/precip.csv')
hist_eta <- read_csv('data/climate/gridmet/eta_kc.csv')
hist_eto <- read_csv('data/climate/gridmet/eto.csv')
hist_climate <- left_join(hist_precip, hist_eta) %>%
  left_join(hist_eto) %>%
  filter(radius == climate_radius) %>%
  # filter(month(date) %in% 4:9) %>%
  mutate(eta_bal = eta-precip,
         eto_bal = eto-precip,
         et_dif = eto - eta) %>%
  group_by(well, year = year(date)) %>%
  summarise(across(c(eta_bal, eto_bal, et_dif, precip, eta, eto), sum))
hist <- left_join(hist_pumping, hist_climate)

hist_lms <- hist %>%
  group_by(well, radius) %>%
  summarise(slp = lm(amt~eta_bal)$coefficients[2],
            int = lm(amt~eta_bal)$coefficients[1],
            r2 = summary(lm(amt~eta_bal))$r.squared)
summary(hist_lms$r2[hist_lms$radius == 4])

ggplot(subset(hist, well == 'dg01' & radius == 4), aes(x = eta_bal, y = amt)) +
  geom_smooth(method = 'lm', se = F) +
  geom_point() +
  facet_wrap(vars(radius), scales = 'free')

# apply to climate projections --------------------------------------------
proj_dirs <- list.dirs('data/climate/maca', recursive = F)

for(i in 1:length(proj_dirs)){
  proj_dir <- proj_dirs[i]
  scenario <- word(proj_dir, -1, sep = '/')
  save_dir <- paste0('data/pumping/projections/',scenario)
  if(!dir.exists(save_dir)) dir.create(save_dir)

  proj_precip <- read_csv(paste0(proj_dir,'/precip.csv'))
  proj_eta <- read_csv(paste0(proj_dir,'/eta_kc.csv'))
  proj_climate <- left_join(proj_precip, proj_eta) %>%
    filter(radius == climate_radius) %>%
    mutate(eta_bal = eta-precip) %>%
    group_by(well, year = year(date)) %>%
    summarise(eta_bal = sum(eta_bal))
  
  proj_annual_pumping <- full_join(proj_climate, hist_lms, relationship = 'many-to-many') %>%
    mutate(use = 'IRR',
           amt = (eta_bal*slp)+int) %>%
    mutate(amt = ifelse(amt < 0, 0, amt)) %>%
    select(well, radius, use, year, amt) %>%
    arrange(well, radius, year)
  
  write_csv(proj_annual_pumping, paste0(save_dir,'/annual_lm.csv'))
}

comp <- full_join(proj_climate, hist_lms, relationship = 'many-to-many') %>%
  mutate(use = 'IRR',
         amt = (eta_bal*slp)+int) %>%
  mutate(amt = ifelse(amt < 0, 0, amt)) %>%
  select(well, year, radius, eta_bal, amt) %>%
  mutate(set = 'proj')
hist_join <- select(hist, well, year, radius, eta_bal, amt) %>%
  mutate(set = 'hist')
comp <- rbind(comp, hist_join)

ggplot(subset(comp, well == 'dg01' & radius == 4), aes(x = eta_bal, y = amt, color = set)) +
  geom_smooth(data = subset(comp, well == 'dg01' & radius == 4 & set == 'hist'),
              method = 'lm', se = F, color = 'black', linetype = 'dashed') +
  geom_point()
