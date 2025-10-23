library(terra)
library(tidyterra)
library(sf)
library(tidyverse)
source('./scripts/functions/Theme+Settings.R')

wells <- read_csv('./data/wells/well_info.csv') %>%
  st_as_sf(., coords = c('long','lat'), crs = 4269)

rad <- 2
units(rad) <- 'km'
well_circles <- st_buffer(wells, dist = rad)

kraa_boundary <- st_read('./data/shapefiles/kraa/kaw_valley_dtf.shp')

precip <- rast('./data/climate/gridmet/precip.tif')[['pr_2016-05-26']]
et <- rast('./data/climate/gridmet/eto.tif')[['pet_2016-05-26']]

ggplot() +
  geom_spatraster(data = et) +
  geom_sf(data = kraa_boundary, fill = NA, color = 'black', linetype = 'dashed', linewidth = 1) +
  geom_sf(data = wells, shape = 22, fill = 'red', color = 'black', size = 2) +
  geom_sf(data = well_circles, alpha = 0.5) +
  scale_fill_viridis_c(name = 'ETo', option = 'A', direction = 1) +
  theme(legend.position = 'right')
ggsave('./figures/rasters/eto_radii.tif', height = 3, width = 8, units = 'in')