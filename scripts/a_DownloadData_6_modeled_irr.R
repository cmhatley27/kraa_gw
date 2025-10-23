# data --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
source('scripts/functions/theme.R')

kraa <- st_read('data/shapefiles/kraa/kaw_valley_dtf.shp') %>% st_transform(5070)
wells <- read_csv('data/wells/well_info.csv') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>% st_transform(5070)

#get grid coords
grid <- read_csv('data/dssat/Coordinates_All_Grid_soil_texture_EKSRB.csv')
#join coords onto full model output and set to a terra vector object
mod <- read_csv('data/dssat/summary_Baseline_EKSRB.csv') %>%
  left_join(grid) %>%
  mutate(year = str_sub(as.character(HDAT), 1, 4)) %>%
  st_as_sf(coords = c('centx', 'centy'), crs = 4269) %>% 
  st_transform(5070) %>%
  vect(.)

#load precip raster to set up model raster
precip <- rast('data/climate/gridmet/precip.tif') %>%
  project('epsg:5070')

#set up template raster to convert model output (points) to raster
#use extent of the model output points and resolution of the precip raster
template_rast <- rast(ext = ext(mod), res = res(precip))

radii <- seq(0.5, 8, by = 0.5)

mod_irr <- data.frame()
count = 0
for(i in 1:length(unique(mod$year))){
  year_sel <- unique(mod$year)[i]
  
  mod_rast <- rasterize(mod[mod$year == year_sel], template_rast, field = 'IRCM') %>%
    subst(NA, 0)
  crs(mod_rast) <- crs(precip)
  
  for(j in 1:length(radii)){
    radius_sel <- radii[j]
    
    well_buffer <- st_buffer(wells, radius_sel*1000)
    
    irr <- extract(mod_rast, vect(well_buffer), 'table', exact = T) %>%
      pivot_longer(!ID, names_to = 'amt', values_to = 'frac') %>%
      mutate(amt = as.numeric(amt),
             amt_weight = amt*frac) %>%
      group_by(ID) %>%
      summarise(amt = sum(amt_weight, na.rm = T))
    model_irr_m <- irr$amt/1000
    model_irr_af <- model_irr_m*pi*((radius_sel*1000)^2)/1233
    
    mod_irr_ij <- data.frame(
      well = wells$well,
      radius = radius_sel,
      use = 'IRR',
      year = year_sel,
      amt = irr$amt
    )
    mod_irr <- rbind(mod_irr, mod_irr_ij)
    
    count = count+1
    print(paste0('Well/radius combo ',count,'/',length(unique(mod$year))*length(radii),' done!'))
  }
}
write_csv(mod_irr, './data/pumping/dssat/annual.csv')
