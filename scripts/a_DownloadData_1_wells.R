# Load libraries and gage info --------------------------------------------
library(tidyverse)
library(zoo)
source('scripts/functions/utilities.R')

well_info <- read_csv('./data/wells/well_info.csv',
                      col_types = cols(prev_well = 'c'))

#download previous well info if not already downloaded
if(file.exists('./data/wells/prev_well_info.csv')){
  prev_well_info <- read_csv('./data/wells/prev_well_info.csv')
} else{
  library(dataRetrieval)
  prev_wells <- well_info$prev_well[!is.na(well_info$prev_well)]
  prev_well_info <- readNWISsite(prev_wells) %>%
    select(c(usgs_id = site_no, lat = dec_lat_va, lon = dec_long_va, elev = alt_va)) %>%
    left_join(select(well_info, c(index_well = well, usgs_id = prev_well)))
  write_csv(site_info, './data/wells/prev_well_info.csv')
}

# Clean raw data ----------------------------------------------------------
# No easy way to automate download of index well data from KGS site. Have to
# manually go to each well's 'Continuous Measurements' page, change the date
# range to the whole record, and hit 'Download to file' to get the raw .txt

# KGS well data here: https://www.kgs.ku.edu/Hydro/KansasRiver/index.html

# After downloading each .txt, this script loops through them and prepares
# the raw data for input into the models.
for(well in 1:nrow(well_info)){
  # set file paths of downloaded data + outputs
  raw_fp <- file.path('data', 'wells', 'raw', paste0(well_info$well[well],'.txt'))
  clean_fp <- file.path('data', 'wells', 'clean', '15min', paste0(well_info$well[well],'.csv'))
  clean_daily_fp <- file.path('data', 'wells', 'clean', 'daily', paste0(well_info$well[well],'.csv'))
  mean_center_fp <- file.path('data', 'wells', 'clean', 'daily_mean_center', paste0(well_info$well[well],'.csv'))
  
  clean <- read_csv(raw_fp) %>%
    select(dateTime = MEASUREMENT_DATE_AND_TIME,
           dtw = DEPTH_TO_WATER) %>%
    # adjust dateTimes round them all to nearest hour
    mutate(dateTime = timechange::time_force_tz(dateTime, tz = 'America/Chicago'),
           dateTime = round_date(dateTime, '1 hour'),
           dateTime = as_datetime(dateTime),
           #convert dtw to groundwater elevation using well elevation (ft), then convert to meters
           gwl = (well_info$elev[well] - dtw)*.3048,
           well = well_info$well[well],
           # flag measurements that appear on duplicate dateTimes because of daylight
           # savings (?). Remove the second appearance of each of these duplicates
           duplicate = ifelse(dateTime == lag(dateTime, default = ymd('1900-01-01')), T, F),
           #flag sampling points as those that are > 1 meter below the previous
           sample = ifelse(lag(gwl, default = gwl[1]) - gwl > 1, T, F)) %>%
    filter(!duplicate & !sample) %>%
    select(dateTime, gwl, well)
  
  # populate missing measurement points with NAs
  clean_fill <- fillinator(clean)
  write_csv(clean_fill, clean_fp)
  # aggregate to daily. Calculate as daily mean when there <= 3 consecutive
  # missing values (hours) in a day. Leave as NA when there are > 3 consec. missings
  clean_daily <- clean_fill %>%
    mutate(gwl = na.approx(gwl, maxgap = 3)) %>%
    group_by(date = date(dateTime)) %>%
    summarise(gwl = mean(gwl)) %>%
    mutate(well = well_info$well[well],
           type = 'index')
  
  # Load in data from previous nearby wells where available. Again no easy way
  # to automate the download of this data so they were manually retrieved from
  # the KGS site (https://geohydro.kgs.ku.edu/geohydro/wizard/) using the 
  # USGS well ID numbers listed in the well_info table.
  if(!is.na(well_info$prev_well[well])){
    prev_well_id <- as.character(well_info$prev_well[well])
    prev_well_elev <- prev_well_info$elev[as.character(prev_well_info$usgs_id) == prev_well_id]
    
    prev_well_dat <- read_csv(paste0('./data/wells/raw/prev_wells/',prev_well_id,'.txt')) %>%
      mutate(date = mdy(measurement_date),
             gwl = (prev_well_elev - depth_to_water)*0.3048,
             well = well_info$well[well],
             type = 'prev',
             .keep = 'none') %>%
      #remove some weird duplicate measurements
      filter(date != lag(date))

    clean_daily <- rbind(clean_daily, prev_well_dat) %>%
      arrange(date)
  }
  write_csv(clean_daily, clean_daily_fp)
  
  mean_center <- clean_daily %>%
    group_by(type) %>%
    mutate(gwl = gwl-mean(gwl, na.rm = T))
  write_csv(mean_center, mean_center_fp)
}
