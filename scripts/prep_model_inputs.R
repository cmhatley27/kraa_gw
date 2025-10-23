library(tidyverse)


# choose well and settings ------------------------------------------------
well_sel <- 'dg01'
save_dir <- paste0('./data/model/',well_sel,'/')

#resample well level data to monthly?
resample <- F
# dates to trim all time series to based on data availability
start_date <- '1981-01-01'
end_date <- '2023-12-31'
# end of projection period
proj_end_date <- '2099-12-31'
# radius around each well (in km) to aggregate variables over
climate_radius <- 2
pumping_radius <- 2
#seasons for disaggregating annual pumping amounts into daily amounts
irr_season <- yday(c('2016-05-01', '2016-09-30'))
mun_season <- yday(c('2016-01-01', '2016-12-31'))
ind_season <- yday(c('2016-01-01', '2016-12-31'))

proj <- T
model <- 'ccsm4'
scenario <- 'rcp85'
proj_dir <- paste0('./data/climate/maca/',model,'_',scenario,'/')

# gwl ---------------------------------------------------------------------
well <- read_csv(paste0('./data/wells/clean/daily/',well_sel,'.csv')) %>%
  arrange(date) %>%
  select(date, gwl) %>%
  filter(date >= start_date & date <= end_date) %>%
  #some duplicates dates for some reason. Remove these
  summarise(gwl = mean(gwl), .by = date)

#optional step to retain only 1 gwl observation per month. Generally improves
#model performance over the original ~daily time step. Pastas has a feature
#that performs this sort of resampling at the time of model calibration, however
#which is more flexible and preferred to doing it here.
if(resample == T){
  well <- well %>% 
    group_by(year = year(date), month = month(date)) %>%
    summarise(gwl = head(gwl, 1),
              date = head(date, 1)) %>%
    ungroup() %>%
    select(date, gwl)
}

write_csv(well, paste0(save_dir,'gwl.csv'))


# precip ------------------------------------------------------------------
precip <- read_csv('./data/climate/gridmet/precip.csv') %>%
  filter(well == well_sel & radius == climate_radius) %>%
  select(date, precip) %>%
  filter(date >= start_date & date <= end_date)

if(proj == T){
precip_proj <- read_csv(paste0(proj_dir,'precip.csv')) %>%
  filter(well == well_sel & radius == climate_radius) %>%
  select(date, precip) %>%
  filter(date > end_date)
precip <- rbind(precip, precip_proj)
}


write_csv(precip, paste0(save_dir,'precip.csv'))

# et ----------------------------------------------------------------------
eta <- read_csv('./data/climate/openet/eta.csv') %>%
  mutate(year = year(date), month = month(date))
eto <- read_csv('./data/climate/gridmet/eto.csv') %>%
  mutate(year = year(date), month = month(date))

et_comb <- left_join(eto, select(eta, !date)) %>%
  filter(well == well_sel & radius == climate_radius)

# aggregate to month to calculate monthly Kc values
et_month <- et_comb %>%
  group_by(year, month) %>%
  summarise(eto = sum(eto),
            eta = mean(eta)) %>%
  mutate(kc = eta/eto)

# extract mean Kc values for each month
kc <- group_by(et_month, month) %>%
  summarise(kc = mean(kc, na.rm = T))

# adjust each daily ETr value using that month's Kc
et_kc <- left_join(et_comb, kc) %>%
  mutate(eta_est = eto*kc) %>%
  select(date, eta = eta_est) %>%
  filter(date >= start_date & date <= end_date)

if(proj == T){
et_proj <- read_csv(paste0(proj_dir,'eto.csv')) %>%
  filter(well == well_sel & radius == climate_radius) %>%
  mutate(month = month(date)) %>%
  left_join(., kc) %>%
  mutate(eta = eto*kc) %>%
  select(date, eta) %>%
  filter(date > end_date)
et_kc <- rbind(et_kc, et_proj)
}

write_csv(et_kc, paste0(save_dir,'et.csv'))


# pumping -----------------------------------------------------------------
#for now only look at irrigation pumping because other uses will be much more
#challenging to project into the future, + they don't correlate with gwl that well.
pumping_annual <- read_csv('./data/pumping/well_annuals.csv') %>%
  filter(well == well_sel & radius == pumping_radius,
         use == 'IRR')

if(proj == T){
pumping_proj <- read_csv(paste0('./data/pumping/',model,'_',scenario,'/',well_sel,'_projection.csv')) %>%
  mutate(use = 'IRR', radius = pumping_radius, well = well_sel) %>%
  select(well, year, use, amt, radius) %>%
  filter(year > pumping_annual$year[length(pumping_annual$year)])
pumping_annual <- rbind(pumping_annual, pumping_proj)
}

#generate daily time series by spreading annual pumping amounts evenly across
#the pumping seasons defined in the first section
pumping_daily <- tibble(
  date = seq.Date(from = ymd(start_date), to = ymd(proj_end_date), by = '1 day'),
  year = year(date),
  yday = yday(date)
)  %>%
  left_join(., pivot_wider(pumping_annual, id_cols = year, names_from = use, values_from = amt))
if('IRR' %in% colnames(pumping_daily)){
  pumping_daily <- mutate(pumping_daily, irr = ifelse(yday < irr_season[1] | yday > irr_season[2],
                                                      0, IRR/(irr_season[2]-irr_season[1])))
}else(pumping_daily$irr = 0)
if('MUN' %in% colnames(pumping_daily)){
  pumping_daily <- mutate(pumping_daily, mun = ifelse(yday < mun_season[1] | yday > mun_season[2],
                                                      0, MUN/(mun_season[2]-mun_season[1])))
}else(pumping_daily$mun = 0)
if('IND' %in% colnames(pumping_daily)){
  pumping_daily <- mutate(pumping_daily, ind = ifelse(yday < ind_season[1] | yday > ind_season[2],
                                                      0, IND/(ind_season[2]-ind_season[1])))
}else(pumping_daily$ind = 0)

pumping <- pumping_daily %>%
  mutate(pump = irr + mun + ind) %>%
  select(date, pump) %>%
  filter(date >= start_date)

# ggplot(subset(pumping, year(date) %in% 2010:2022), aes(x = date, y = pump)) +
#   geom_line(color = 'green4') +
#   labs(x = NULL, y = 'Daily Irrigation Pumping [m3]')

write_csv(pumping, paste0(save_dir,'pump.csv'))
