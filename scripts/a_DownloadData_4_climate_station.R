# Load libraries and station info -----------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')

#Found station names from the Open File Report on each time series chart (Figures 3 - 22),
#then went to NOAA's online data portal (https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND)
#and searched up the station names to get their station ID code.

station_codes <- c(
  'Manhattan_Airport' = 'USW00003936',
  'Wamego1' = 'USC00148563',
  'Wamego2' = 'US1KSPT0044',
  'Rossville' = 'USC00147007',
  'Topeka_Billard_Airport' = 'USW00013996',
  'Lecompton' = 'USC00144613',
  'Lawrence_Airport' = 'USW00003997',
  'Eudora' = 'USC00142620',
  'Shawnee' = 'US1KSJO0065'
)

# Download raw station data directly from NOAA https address --------------
for(station in 1:length(station_codes)){
  raw_fp <- file.path('data', 'climate', 'met_station', 'raw', paste0(names(station_codes)[station],'.csv'))
  met_raw <- read_csv(paste0('https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/',station_codes[station],'.csv'))
  write_csv(met_raw, raw_fp)
}

# Clean -------------------------------------------------------------------

#trim all series to start when gridMET climate data is available
trim_date <- '1979-01-01'

for(station in 1:length(station_codes)){
  raw_fp <- file.path('data', 'climate', 'met_station', 'raw', paste0(names(station_codes)[station],'.csv'))
  clean_fp <- file.path('data', 'climate', 'met_station', 'clean', paste0(names(station_codes)[station],'.csv'))
  
  # trim dates, rename columns, and convert from 10ths of mm/degrees
  clean <- read_csv(raw_fp) %>%
    filter(DATE >= trim_date)
  
    if('TMIN' %in% colnames(clean)){
    clean <- clean %>%
      mutate(precip = PRCP/10,
      tmin = TMIN/10,
      tmax = TMAX/10,
      tavg = (tmin + tmax)/2) %>%
      select(date = DATE, precip, tavg, tmin, tmax)
    }else{
      clean <- clean %>%
        mutate(precip = PRCP/10) %>%
        select(date = DATE, precip)
    }
  write_csv(clean, clean_fp)
}