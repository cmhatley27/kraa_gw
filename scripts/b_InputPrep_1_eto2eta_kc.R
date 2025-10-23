library(tidyverse)

# establish monthly crop factors in historical period ---------------------
save = F

eto <- read_csv('./data/climate/gridmet/eto.csv') %>%
  mutate(year = year(date), month = month(date))
eta <- read_csv('./data/climate/openet/eta.csv') %>%
  mutate(year = year(date), month = month(date))

eto_monthly <- eto %>%
  group_by(well, radius, year = year(date), month = month(date)) %>%
  summarise(eto = sum(eto))

et_comb <- left_join(eto_monthly, eta) %>%
  mutate(kc = eta/eto)

kc <- et_comb %>%
  group_by(well, radius, month) %>%
  summarise(kc = mean(kc, na.rm = T))

eta_kc <- left_join(eto, kc) %>%
  mutate(eta = eto*kc) %>%
  select(well, radius, date, eta)

if(save == T) write_csv(eta_kc, 'data/climate/gridmet/eta_kc.csv')

# apply to projections ----------------------------------------------------
proj_dirs <- list.dirs('data/climate/maca', recursive = F)
eto_proj_files <- list.files('data/climate/maca', pattern = 'eto.csv',
                             recursive = T, full.names = T)

for(i in 1:length(proj_dirs)){
  proj_dir <- proj_dirs[i]
  proj_eto <- read_csv(paste0(proj_dir, '/eto.csv')) %>%
    mutate(year = year(date), month = month(date))
  
  proj_eta_kc <- left_join(proj_eto, kc) %>%
    mutate(eta = eto*kc) %>%
    select(well, radius, date, eta)
  
  write_csv(proj_eta_kc, paste0(proj_dir,'/eta_kc.csv'))
}
