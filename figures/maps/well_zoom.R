# data --------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(ggspatial)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')

lc <- rast('data/land_use/ksrb_lulc_NLCD_Land_Cover_2019.tif') %>%
  project('epsg:5070')
kraa <- st_read('data/shapefiles/kraa/kaw_valley_dtf.shp') %>%
  st_transform(crs(lc))
lc <- crop(lc, kraa)

wells <- read_csv('data/wells/well_info.csv') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(crs(lc))

wimas <- read_csv('data/pumping/wimas/wimas_clean.csv') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(crs(lc)) %>%
  filter(year >= 1981)


# map area around specific well -------------------------------------------
well_sel <- 'dg01'
well <- filter(wells, well == well_sel)

buffer_dist <- 4
units(buffer_dist) <- 'km'
well_buffer <- st_buffer(well, buffer_dist)

zoom_dist <- 10
units(zoom_dist) <- 'km'
zoom_box <- st_buffer(well, zoom_dist)

lc_zoom <- crop(lc, zoom_box)
kraa_zoom <- st_crop(kraa, zoom_box)
wimas_zoom <- st_crop(wimas, zoom_box)

ggplot() +
  geom_spatraster(data = lc_zoom, show.legend = F, alpha = 0.25) +
  geom_sf(data = kraa_zoom, fill = NA) +
  geom_sf(data = well) +
  geom_sf(data = well_buffer, fill = NA, linetype = 'dashed', linewidth = 1) +
  geom_sf(data = wimas_zoom, aes(color = use)) +
  scale_color_manual(limits = c('IRR'), values = '#00BA38', na.value = NA) +
  theme(axis.text = element_blank())



# with DSSAT --------------------------------------------------------------
year_sel <- 2012

#get grid coords
grid <- read_csv('data/dssat/Coordinates_All_Grid_soil_texture_EKSRB.csv')
#join coords onto full model output
mod <- read_csv('data/dssat/summary_Baseline_EKSRB.csv') %>%
  left_join(grid) %>%
  mutate(year = str_sub(as.character(HDAT), 1, 4),
         hyday = as.numeric(str_sub(as.character(HDAT), 5,7)),
         pyday = hyday - NDCH,
         hdate = ymd('1997-12-31') + days(hyday),
         pdate = ymd('1997-12-31') + days(pyday)) %>%
  st_as_sf(coords = c('centx', 'centy'), crs = 4269) %>% st_transform(5070)

#set up template raster to convert model output (points) to raster
#use extent of the model output points and resolution of the precip raster
precip <- rast('data/climate/gridmet/precip.tif') %>% project('epsg:5070')
template_rast <- rast(ext = ext(mod), res = res(precip))
#rasterize points, selecting an individual year
mod_vect <- vect(mod)
mod_rast <- rasterize(mod_vect[mod_vect$year == year_sel], template_rast, field = 'IRCM')
crs(mod_rast) <- crs(precip)
mod_zoom <- mask(mod_rast, kraa) %>%
  crop(., zoom_box)

well_buffer2 <- st_buffer(well, 2000)
well_buffer6 <- st_buffer(well, 6000)

ggplot() +
  geom_spatraster(data = mod_zoom, show.legend = T, alpha = 1) +
  geom_sf(data = kraa_zoom, fill = NA) +
  geom_sf(data = well) +
  geom_sf(data = wimas_zoom, aes(color = use)) +
  geom_sf(data = well_buffer2, fill = NA, linetype = 'dashed', linewidth = 1, color = 'black') +
  geom_sf(data = well_buffer, fill = NA, linetype = 'dashed', linewidth = 1, color = 'black') +
  geom_sf(data = well_buffer6, fill = NA, linetype = 'dashed', linewidth = 1, color = 'black') +
  scale_fill_viridis_c(na.value = NA, name = 'Irr', guide = 'none') +
  scale_color_manual(limits = c('IRR'), values = '#00BA38', na.value = NA,
                     guide = 'none') +
  theme(axis.text = element_blank())
