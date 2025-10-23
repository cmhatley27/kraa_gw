# download MACA projection rasters ----------------------------------------
# MACA includes all of the meteorological variables from gridMET but does not
# include the derived ETo. Need to do it myself here. Using the FAO ETo
# quation described in https://www.fao.org/4/x0490e/x0490e06.htm and its
# implementation in the package 'FAO56'.

# There are some slight discrepancies between the ET values calculated here
# and what is available from gridMET (see code section at bottom), but they
# are similar enough that I'm not expecting any major issues. Might revisit
# this later though.
library(tidyverse)
library(sf)
library(terra)
library(climateR)
library(AOI)
library(FAO56)
source('scripts/functions/utilities.R')

#select gcm model and scenario
model <- 'ccsm4' #ccsm4, canesm, gfdl, cnrm, bcc
scenario <- 'rcp45' #rcp45 or rcp85
save_dir <- paste0('./data/climate/maca/',model,'_',scenario,'/')
if(!dir.exists(save_dir)) dir.create(save_dir)

# download MACA data. Need precip, temp, wind speed, and solar radiation----
bbox <- st_bbox(c(xmin = -96.90161, xmax = -94.49292,
                  ymax = 39.3573, ymin = 38.82929),
                crs = 4269) %>%
  bbox_get(.)

dat <- getMACA(AOI = bbox,
               model = gcm_lookup(model),
               scenario = scenario,
               varname = 'asdf',#c('pr','tasmin','tasmax','uas','vas','rsds'),
               startDate = '2006-01-01',
               endDate = '2099-12-31')
dat$precipitation <- project(dat$precipitation, 'epsg:4269')
dat$surface_downwelling_shortwave_flux_in_air <- project(dat$surface_downwelling_shortwave_flux_in_air, 'epsg:4269')
dat$northward_wind <- project(dat$northward_wind, 'epsg:4269')
dat$eastward_wind <- project(dat$eastward_wind, 'epsg:4269')

writeRaster(dat$precipitation, paste0(save_dir,'precip.tif'))
writeRaster(dat$surface_downwelling_shortwave_flux_in_air, paste0(save_dir,'srad.tif'))
writeRaster(dat$northward_wind, paste0(save_dir,'nwind.tif'))
writeRaster(dat$eastward_wind, paste0(save_dir,'ewind.tif'))

#min and max temperature get downloaded as one raster stack AND for some reason
#min and max temperatures are swapped (i.e. rasters named 'tasmin' actually
#contain max temps) in CCSM4 RCP4.5
mintemp1 <- dat$air_temperature[1][str_detect(names(dat$air_temperature), 'tasmin')][1]
maxtemp1 <- dat$air_temperature[1][str_detect(names(dat$air_temperature), 'tasmax')][1]

if(maxtemp1 >= mintemp1){
  dat$max_temp <- subset(dat$air_temperature, str_detect(names(dat$air_temperature), 'tasmax'))
  dat$min_temp <- subset(dat$air_temperature, str_detect(names(dat$air_temperature), 'tasmin'))
} else {
  dat$max_temp <- subset(dat$air_temperature, str_detect(names(dat$air_temperature), 'tasmin'))
  dat$min_temp <- subset(dat$air_temperature, str_detect(names(dat$air_temperature), 'tasmax'))
}

dat$max_temp <- project(dat$max_temp, 'epsg:4269')
dat$min_temp <- project(dat$min_temp, 'epsg:4269')

writeRaster(dat$max_temp, paste0(save_dir,'maxtemp.tif'))
writeRaster(dat$min_temp, paste0(save_dir,'mintemp.tif'))

# Need elevation to derive atmospheric pressure in ET calculation. Elev raster
# available on the downloads page of https://www.climatologylab.org/gridmet.html
if(!file.exists('./data/climate/maca/elev.tif')){
  elev <- rast('./data/climate/maca/elev.nc') %>%
    crop(., precip, touches = F)
  writeRaster(elev, './data/climate/maca/elev.tif', overwrite = T)
}

# Calculate Penman Montieth ETo -------------------------------------------
#load temps in degrees C instead of K
mintemp <- rast(paste0(save_dir,'mintemp.tif')) - 273.15
maxtemp <- rast(paste0(save_dir,'maxtemp.tif')) - 273.15
#load solar radiation in MJ/m2/day instead of W/m2
srad <- rast(paste0(save_dir,'srad.tif'))/1000000*60*60*24
#load wind as the pythagorean distance from north and east components
wind <- sqrt((rast(paste0(save_dir,'nwind.tif'))^2) + (rast(paste0(save_dir,'ewind.tif'))^2))
elev <- rast('./data/climate/maca/elev.tif')

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

writeRaster(eto, paste0(save_dir,'eto.tif'), overwrite = T)


# calculate spatial means around each well ----------------------------------
precip <- rast(paste0(save_dir,'precip.tif'))
eto <- rast(paste0(save_dir,'eto.tif'))

well_info <- read_csv('./data/wells/well_info.csv') %>%
  st_as_sf(., coords = c('lon','lat'), crs = 4269)

#set which radii around the wells we want to get ET means from 
radii <- c(0.1,0.25,0.5,1,2,4,8)

eto_means <- radial_means(eto,
                          well_info,
                          radii,
                          'eto') %>%
  mutate(eto = ifelse(eto < 0, 0, eto))
precip_means <- radial_means(precip,
                             well_info,
                             radii,
                             'precip')

write_csv(eto_means, paste0(save_dir,'eto.csv'))
write_csv(precip_means, paste0(save_dir,'precip.csv'))