library(tidyverse)

irr_files <- c('data/pumping/wimas/annual.csv',
               'data/pumping/dssat/annual.csv',
               paste0(list.dirs('data/pumping/projections', recursive = F), '/annual_lm.csv'))

#define the irrigation season over which pumping amounts will be evenly spread
irr_season <- yday(c('2000-05-01', '2000-09-30'))
irr_season_length <- irr_season[2] - irr_season[1]

# disaggregate pumping ------------------------------------------------------
for(i in 1:length(irr_files)){
  irr_file <- irr_files[i]
  irr_dir <- paste0(word(irr_files[1],1,-2, sep = '/'),'/')
  
  irr_annual <- read_csv(irr_file) %>%
    filter(year >= 1981) %>%
    filter(use == 'IRR')
  
  irr_start_date <- ymd(paste0(min(unique(irr_annual$year)),'-01-01'))
  irr_end_date <- ymd(paste0(max(unique(irr_annual$year)),'-12-31'))
  date_seq <- seq.Date(irr_start_date, irr_end_date, by = 'day')
  
  irr_daily <- tibble(date = date_seq,
                      year = year(date_seq)) %>%
    left_join(irr_annual, relationship = 'many-to-many') %>%
    mutate(amt = ifelse(yday(date) >= irr_season[1] & yday(date) <= irr_season[2],
                        amt/irr_season_length,
                        0)) %>%
    select(well, radius, use, date, amt) %>%
    arrange(well, radius, date)
  
  write_csv(irr_daily, str_replace(irr_file, 'annual', 'daily_even'))
}
