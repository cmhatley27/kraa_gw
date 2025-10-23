# load bits ---------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(ggspatial)
source(file.path('scripts','functions','theme.R'))

kraa_boundary <- read_sf(file.path('data','shapefiles','kraa','kaw_valley_dtf.shp'))[1,] %>%
  st_transform(., crs = 4269)

eksrb_boundary <- read_sf(file.path('data','shapefiles','eksrb','watershed_bndry.shp'))[1,] %>%
  st_transform(., crs = 4269)

wells <- read_csv(file.path('data','wells','well_info.csv')) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = st_crs(4269))

prev_wells <- read_csv(file.path('data','wells','prev_well_info.csv')) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = st_crs(4269))

met_stations <- read_csv(file.path('data','climate','met_station','met_station_info.csv')) %>%
  st_as_sf(coords = c('long', 'lat'), crs = st_crs(4269))

# gages <- read_csv(file.path('data','river','gage_locations.csv')) %>%
#   st_as_sf(coords = c('lon', 'lat'), crs = st_crs(4269))

ggplot() +
  geom_sf(data = kraa_boundary) +
  geom_sf(data = wells, color = 'red') +
  geom_sf(data = prev_wells, color = 'green')
  geom_sf(data = met_stations, color = 'blue')

# raw land use ------------------------------------------------------------
lulc <- rast(file.path('data', 'land_use', 'ksrb_lulc_NLCD_Land_Cover_2019.tif')) %>%
    project(., 'epsg:4269')
lulc_crop <- lulc %>%
  crop(., eksrb_boundary, mask = TRUE)
rm(lulc)

ggplot() +
  geom_spatraster(data = lulc_crop) +
  geom_sf(data = kraa_boundary, fill = NA, color = 'black', linewidth = 1) +
  geom_sf(data = wells)

# ggsave(file.path('figures','study_area','lulc.png'), height = 8, width = 12, units = 'in')

# recode land use ---------------------------------------------------------

#change pasture to same code as grassland
tab <- coltab(lulc_crop)[[1]]
tab_recode <- tab
tab_recode[82,2:4] <- tab[72,2:4]

lulc_crop_recode <- lulc_crop
coltab(lulc_crop_recode) <- tab_recode

ggplot() +
  geom_spatraster(data = lulc_crop_recode) +
  geom_sf(data = eksrb_boundary, fill = NA, color = 'black', linewidth = 1) +
  geom_sf(data = kraa_boundary, fill = NA, color = 'black', linewidth = 1, linetype = 'dashed') +
  geom_sf(data = wells, shape = 22, color = 'black', fill = 'red', size = 4) +
  geom_sf(data = prev_wells, shape = 23, color = 'black', fill = 'blue', size = 3) +
  scale_fill_coltab(lulc_crop_recode, guide = 'none') +
  theme_void()
  

ggsave(file.path('figures','maps','lulc_prevwells.png'), height = 6, width = 8, units = 'in')

# Extent ------------------------------------------------------------------
states <- vect(file.path('data','shapefiles','states','States_shapefile.shp')) %>%
  filter(State_Code == 'KS')
extent <- vect(ext(eksrb_boundary))

ggplot() +
  geom_sf(data = states, fill = NA, color = 'black', linewidth = 1) +
  # geom_spatraster(data = lulc_crop_recode) +
  geom_sf(data = eksrb_boundary, color = 'black', fill = 'red', alpha = 0.3, linetype = 'dashed') +
  # scale_fill_coltab(lulc_crop_recode, guide = 'none') +
  theme_void()

ggsave(file.path('figures','maps','extent.png'), height = 4, width = 8, units = 'in')
# calc frequencies --------------------------------------------------------

lulc_mask <- mask(lulc_crop, kraa_boundary)

freq_table <- freq(lulc_mask) %>%
  mutate(pct = count/sum(count)*100)
freq_table_agg <- freq_table %>%
  summarise(crop = sum(count[value == 'Cultivated Crops'])/sum(count),
            grass = sum(count[value %in% c('Pasture/Hay', 'Grassland/Herbaceous')])/sum(count),
            developed = sum(count[str_detect(value, 'Developed')])/sum(count),
            forest = sum(count[str_detect(value, 'Forest')])/sum(count))

