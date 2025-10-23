# Load libraries and gage info --------------------------------------------
library(tidyverse)
library(dataRetrieval)
library(zoo)
source('scripts/functions/utilities.R')

gage_info <- read_csv('./data/river/gages/gage_info.csv')

# Download and save raw data -------------------------------------------------------
start_date <- date('1980-01-01')
end_date <- date('2024-09-30')

for(gage in 1:nrow(gage_info)){
  fp <- file.path('data','river','gages','raw',paste0(gage_info$location[gage],'.csv'))
  if(file.exists(fp)) next
  raw <- readNWISdv(gage_info$usgs_code[gage], c("00060", '00065'), start_date, end_date)
  write_csv(raw, fp)
}

# Process and clean raw data ----------------------------------------------
for(gage in 1:nrow(gage_info)){
  raw_fp <- file.path('data','river','gages','raw',paste0(gage_info$location[gage],'.csv'))
  clean_fp <- file.path('data','river','gages','clean',paste0(gage_info$location[gage],'.csv'))
  
  raw <- read_csv(raw_fp)
  if('X_00060_00003' %in% colnames(raw)){
    clean <- raw %>%
      select(date = Date,
             stage = X_00065_00003,
             q = X_00060_00003) %>%
      mutate(id = gage_info$location[gage],
             date = ymd(date),
             #convert from ft to m above sea level
             stage = stage*0.3048,
             #convert from cfs to m3/s
             q = q*0.028316847)
  }else{
    clean <- raw %>%
      select(date = Date,
             stage = X_00065_00003) %>%
      mutate(id = gage_info$location[gage],
             date = ymd(date),
             #convert from ft to m above sea level
             stage = stage*0.3048)
  }
  write_csv(clean, clean_fp)
}
