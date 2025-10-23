# libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(rgee)

# download OpenET rasters -------------------------------------------------
# OpenET data is very fine resolution (30m) and thus it is not really
# feasible to download rasters for the entire study area, even at a monthly
# time step.
# Instead, just use the Google Earth Engine API to extract mean ET values
# in a radius around each well at each time step. Get several radii to test
# different areas of influence
# may need to go to Tools -> Global Options -> Python and set the Python
# interpretor path to the below directory + '/python.exe'
reticulate::use_condaenv('C:/Users/cmhat/.conda/envs/ee')
ee_Initialize(user = 'cmhatley@gmail.com', drive = T)

#get full image collection of Actual ET
openet <- ee$ImageCollection('OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0') %>%
  ee$ImageCollection$select('et_ensemble_mad')

#get start and ends dates of each image
img_starts <- unique(ee_get_date_ic(openet)$time_start)
img_ends <- lead(img_starts) - days(1)
img_ends[length(img_ends)] <- img_starts[length(img_starts)] + days(30)

#load well locations
well_info <- read_csv('./data/wells/well_info.csv') %>%
  st_as_sf(., coords = c('lon','lat'), crs = 4269)

#set which radii around the wells we want to get ET means from 
radii <- c(0.1,0.25,0.5,1,2,4,8)

#set up results dataframe
eta_means <- tibble(well = NA,
                    eta = NA,
                    date = NA,
                    radius = NA,
                    .rows = length(radii)*length(img_starts)*length(well_info$well))

for(r in 1:length(radii)){
  #loop through different radii around each well
  well_radius <- radii[r]
  units(well_radius) <- 'km'
  well_circles <- st_buffer(well_info, well_radius) %>%
    select(., c(well, geometry))
  
  for(date in 1:length(img_starts)){
    #loop through each date in the record
    batch_index <- (r-1)*length(img_starts)*length(well_info$well) + (date-1)*length(well_info$well) + 1
    
    openet_month <- openet$filterDate(img_starts[date], img_ends[date])$mean()
    
    eta <- ee_extract(openet_month, well_circles, scale = 30) %>%
      rename(eta = 2) %>%
      mutate(date = img_starts[date],
             radius = as.numeric(well_radius))
    
    
    eta_means[batch_index:(batch_index + length(well_info$well) - 1),] <- eta
    
    print(paste0('batch ',(r-1)*length(img_starts) + date,'/',length(radii)*length(img_starts),' done!!'))
  }
}
write_csv(eta_means, './data/climate/openet/eta.csv')