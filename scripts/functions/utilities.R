#quickly save the most recent ggplot to a temp directory
qsave <- function(height = 5, width = 6){
  return(ggsave('./figures/temp.png', height = height, width = width, units = 'in'))
}

# extract spatial means of a raster in radii around points
radial_means <- function(x, y, r, val_name){
  dat_radial_mean_all <- tibble()
  for(radius in 1:length(r)){
    well_radius <- r[radius]
    units(well_radius) <- 'km'
    well_circles <- st_buffer(y, well_radius) %>%
      select(well, geometry)
    
    dat_radial_mean <- terra::zonal(x, vect(well_circles), fun = 'mean', weights = T) %>%
      cbind(., well = y$well) %>%
      pivot_longer(!well, names_to = 'date', values_to = val_name) %>%
      mutate(radius = as.numeric(well_radius),
             date = ymd(word(date, 2, sep = '_')))
    
    dat_radial_mean_all <- rbind(dat_radial_mean_all, dat_radial_mean)
    
    print(paste0('radius ',radius,'/',length(r),' done!!!'))
  }
  return(dat_radial_mean_all)
}

#data retrieval doesn't return rows for datetimes with missing values. This function
#adds in these missing rows as NA observations.
fillinator <- function(x, date_col = 'dateTime', step = '1 hour'){
  x_fill <- tibble(
    dateTime = seq.POSIXt(x[[date_col]][1], x[[date_col]][length(x[[date_col]])], by = step)
  ) %>%
    left_join(., x)
  return(x_fill)
}

gcm_lookup <- function(x){
  gcm_names <- c(
    'CCSM4' = 'ccsm4', #NCAR
    'CNRM-CM5' = 'cnrm', #france
    'CanESM2' = 'canesm', #canada
    'bcc-csm1-1' = 'bcc', #china
    'GFDL-ESM2M' = 'gfdl' #NOAA
  )
  return(names(gcm_names)[gcm_names == x])
}
