# libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
source('./scripts/functions/utilities.R')

# clean WIMAS -------------------------------------------------------------
#load kraa shapefile
kraa <- st_read('data/shapefiles/kraa/kaw_valley_dtf.shp') %>%
  st_transform(., 4269)
st_bbox(kraa)+c(-0.1,-0.1,0.1,0.1) #get bounding box with a small buffer
# bounding box (with buffer): N=39.35730, E=-94.49292, S=38.82929, W=-96.90161

# go to https://geohydro.kgs.ku.edu/geohydro/wimas/query_setup.cfm and enter
# bounding box coordinates

# Click 'Water Use Trends' at the top right, then in the left box enter a date 
# range and click 'Submit/Download Trend Request'.

# Finally, click 'Download Water Use History by Water Right' in the box that
# pops up to get a .txt file containing water use data. It can take several
# minutes for the .txt to generate.

wimas_raw <- read_csv('./data/pumping/wimas/wimas_raw.txt')

# wimas_use has a lot of unnecessary columns and some weird duplicate rows, 
# so we need to remove those to clean it up
wimas_clean <- wimas_raw %>%
  # select only relevant columns
  select(pdiv_id, source, year = wua_year, use = umw_code, amt = af_used,
         crop_code, acres_irr, lat = latitude, lon = longitude) %>%
  # remove duplicate rows by grouping by each unique combiniation of pdiv,
  # year, source (groundwater or surface), and sector (IRR, MUN, etc.) and summarizing
  # the water use, crop code, and acres irrigated data
  group_by(pdiv_id, year, source, use, lat, lon) %>%
  summarise(amt = sum(amt, na.rm = T),
            crop_code = crop_code[!is.na(crop_code)][1],
            acres_irr = median(acres_irr, na.rm = T)) %>%
  mutate(mm = (amt/acres_irr)*304.8) %>%
  mutate(mm = ifelse(is.infinite(mm), NA, mm)) %>%
  arrange(pdiv_id, year, use)

write_csv(wimas_clean, 'data/pumping/wimas/wimas_clean.csv')


# Aggregate pumping in different radii around each well -------------------
wimas <- read_csv('data/pumping/wimas/wimas_clean.csv') %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = 4269)

well_info <- read_csv('data/wells/well_info.csv') %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = 4269)

radii <- seq(0.5, 8, by = 0.5)

pumping_totals <- tibble()
for(w in 1:nrow(well_info)){
  well_sel <- well_info$well[w]
  wimas_dist <- wimas %>%
    mutate(dist = st_distance(., well_info[well_info$well == well_sel,])[,1])
  
  for(r in 1:length(radii)){
    radius <- radii[r]
    units(radius) <- 'km'
    
    wimas_fil <- filter(wimas_dist, dist <= radius)
    wimas_sum <- wimas_fil %>%
      filter(use %in% c('IRR','MUN','IND')) %>%
      group_by(year, use) %>%
      summarise(amt = sum(amt),
                mm = sum(mm, na.rm = T)) %>%
      mutate(radius = as.numeric(radius),
             well = well_sel) %>%
      select(well, radius, use, year, amt, mm) %>%
      st_drop_geometry(.)
    
    print(paste0('batch ',(w-1)*length(radii) + r,'/',nrow(well_info)*length(radii),' done!!!'))
    
    pumping_totals <- rbind(pumping_totals, wimas_sum)
  }
}

write_csv(pumping_totals, './data/pumping/wimas/annual.csv')
