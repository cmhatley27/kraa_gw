# libraries ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(climateR)
library(AOI)
source('scripts/functions/utilities.R')

# download gridMET rasters ------------------------------------------------
# using a bounding box that is slightly larger than the study area:
# N = 39.3573; E = -94.49292; S = 38.82929; W = -96.90161

bbox <- st_bbox(c(xmin = -96.90161, xmax = -94.49292,
                  ymax = 39.3573, ymin = 38.82929),
                crs = 4269) %>%
  bbox_get(.)

# Download daily precip and grass reference ET
dat <- getGridMET(AOI = bbox,
                  varname = c('pr','tmmn','tmmx','vs','srad','pet'),
                  startDate = '1979-01-01',
                  endDate = '2024-12-31')
dat$precipitation_amount <- project(dat$precipitation_amount, 'epsg:4269')
dat$daily_minimum_temperature <- project(dat$daily_minimum_temperature, 'epsg:4269')
dat$daily_maximum_temperature <- project(dat$daily_maximum_temperature, 'epsg:4269')
dat$daily_mean_wind_speed <- project(dat$daily_mean_wind_speed, 'epsg:4269')
dat$daily_mean_shortwave_radiation_at_surface <- project(dat$daily_mean_shortwave_radiation_at_surface, 'epsg:4269')
dat$daily_mean_reference_evapotranspiration_grass <- project(dat$daily_mean_reference_evapotranspiration_grass, 'epsg:4269')


writeRaster(dat$precipitation_amount, './data/climate/gridmet/precip.tif', overwrite = T)
writeRaster(dat$daily_minimum_temperature, './data/climate/gridmet/tmin.tif', overwrite = T)
writeRaster(dat$daily_maximum_temperature, './data/climate/gridmet/tmax.tif', overwrite = T)
writeRaster(dat$daily_mean_wind_speed, './data/climate/gridmet/wind.tif', overwrite = T)
writeRaster(dat$daily_mean_shortwave_radiation_at_surface, './data/climate/gridmet/srad.tif', overwrite = T)
writeRaster(dat$daily_mean_reference_evapotranspiration_grass, './data/climate/gridmet/pet.tif', overwrite = T)

# Need elevation to derive atmospheric pressure in ET calculation. Elev raster
# available on the downloads page of https://www.climatologylab.org/gridmet.html
if(!file.exists('./data/climate/gridmet/elev.tif')){
  elev <- rast('./data/climate/gridmet/elev.nc') %>%
    project(., 'epsg:4269') %>%
    resample(., dat$precipitation_amount)
  writeRaster(elev, './data/climate/gridmet/elev.tif', overwrite = T)
}

# calculate ETo with FAO P-M ----------------------------------------------
#load temps in degrees C instead of K
mintemp <- rast('data/climate/gridmet/tmin.tif') - 273.15
maxtemp <- rast('data/climate/gridmet/tmax.tif') - 273.15
#load solar radiation in MJ/m2/day instead of W/m2
srad <- rast('data/climate/gridmet/srad.tif')/1000000*60*60*24
#load wind as the pythagorean distance from north and east components
wind <- rast('data/climate/gridmet/wind.tif')
elev <- rast('data/climate/gridmet/elev.tif')

#calc net radiation
#net solar radiation as solar rad - albedo effects
a = 0.23
rad_ns = (1-a)*srad
#actual vapor pressure estimated as the saturation vapor pressure at min temp.
#min temp is used here as an estimate of dewpoint temp.
ea = 0.6108*exp((17.27*mintemp)/(mintemp+273.3))
#stefan boltzman constant
sigma = 4.903*10^-9
#net longwave radiation
rad_nl = sigma*(((mintemp+273.15)^4+(maxtemp+273.15)^4)/2)*(0.34-0.14*sqrt(ea))
#net radiation
rad_net = rad_ns - rad_nl
#remove extra rasters for memory
rm(rad_ns, ea, rad_nl)

eto <- ETo_FPM(T_min = mintemp, T_max = maxtemp, u_2 = wind, R_n = rad_net, elev = elev)

writeRaster(eto, paste0('data/climate/gridmet/eto.tif'), overwrite = T)

# calculate spatial means around each well ------------------------------

precip <- rast('./data/climate/gridmet/precip.tif')
eto <- rast('./data/climate/gridmet/eto.tif')
pet <- rast('./data/climate/gridmet/pet.tif')

well_info <- read_csv('./data/wells/well_info.csv') %>%
  st_as_sf(., coords = c('lon','lat'), crs = 4269)

#set which radii around the wells we want to get ET means from 
radii <- c(0.1,0.25,0.5,1,2,4,8)

eto_means <- radial_means(eto,
                          well_info,
                          radii,
                          'eto')
precip_means <- radial_means(precip,
                          well_info,
                          radii,
                          'precip')
pet_means <- radial_means(pet,
                          well_info,
                          radii,
                          'pet')

write_csv(eto_means, './data/climate/gridmet/eto.csv')
write_csv(precip_means, './data/climate/gridmet/precip.csv')
write_csv(pet_means, './data/climate/gridmet/pet.csv')
