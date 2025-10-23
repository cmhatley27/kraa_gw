# data --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
source('scripts/functions/theme.R')

eksrb <- st_read('data/shapefiles/eksrb/watershed_bndry.shp') %>% st_transform(5070)
kraa <- st_read('data/shapefiles/kraa/kaw_valley_dtf.shp') %>% st_transform(5070)
wells <- read_csv('data/wells/well_info.csv') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>% st_transform(5070)
wimas <- read_csv('data/pumping/wimas/wimas_clean.csv') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>% st_transform(5070) %>%
  st_intersection(., kraa)
wimas_wells <- read_csv('data/pumping/wimas/annual.csv') %>%
  left_join(wells)

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

#load precip raster for grid comparison
precip <- rast('data/climate/gridmet/precip.tif') %>% project('epsg:5070')
start <- which(names(precip) == 'pr_1998-01-01')
end <- which(names(precip) == 'pr_1998-08-28')
precip1998 <- subset(precip, start:end)
sum_precip <- sum(precip1998)

#set up template raster to convert model output (points) to raster
#use extent of the model output points and resolution of the precip raster
template_rast <- rast(ext = ext(mod), res = res(precip))
#rasterize points, selecting an individual year
mod_vect <- vect(mod)


# compare model output to wimas -------------------------------------------
year_sel <- 2012

mod_rast <- rasterize(mod_vect[mod_vect$year == year_sel], template_rast, field = 'IRCM')
crs(mod_rast) <- crs(precip)
mod_kraa <- mask(mod_rast, kraa)
wimas_fil <- filter(wimas, year == year_sel & use == 'IRR')

ggplot() +
  geom_spatraster(data = mod_rast) +
  geom_sf(data = wimas_fil, aes(color = amt)) +
  geom_sf(data = eksrb, fill = NA) +
  geom_sf(data = kraa, fill = NA) +
  scale_fill_viridis_c(na.value = NA, name = 'Irr') +
  scale_color_viridis_c() +
  theme(axis.text = element_blank())

cell_area_m2 <- res(mod_kraa)[1] * res(mod_kraa)[2]
model_irr_m <- global(mod_kraa, 'sum', na.rm = T)[1,1]/1000

model_irr_af <- cell_area_m2*model_irr_m/1233
wimas_irr_af <- sum(wimas_fil$amt)

model_irr_af/wimas_irr_af

# do for all years --------------------------------------------------------
comp <- data.frame(
  year = unique(mod$year),
  wimas = NA,
  model  = NA
)

for(i in 1:nrow(comp)){
  year_sel = comp$year[i]
  
  #set up raster from model output
  mod_rast <- rasterize(mod_vect[mod_vect$year == year_sel], template_rast, field = 'IRCM')
  crs(mod_rast) <- crs(precip)
  mod_kraa <- mask(mod_rast, kraa)
  #get irrigation total from raster
  cell_area_m2 <- res(mod_kraa)[1] * res(mod_kraa)[2]
  model_irr_m <- global(mod_kraa, 'sum', na.rm = T)[1,1]/1000
  comp$model[i] <- cell_area_m2*model_irr_m/1233
  
  #get wimas total direct from data
  wimas_fil <- filter(wimas, year == year_sel & use == 'IRR')
  comp$wimas[i] <- sum(wimas_fil$amt)
}

ggplot(comp, aes(x = wimas, y = model)) +
  geom_point() + 
  geom_abline(slope = 10) +
  ylab('Crop Model') +
  xlab('WIMAS') +
  ggtitle('Annual irrigation amounts in KRAA 1991-2015')
summary(lm(model~wimas, data = comp))

# do for individual wells -------------------------------------------------
comp <- data.frame()
for(i in 1:length(unique(mod$year))){
  year_sel <- unique(mod$year)[i]
  
  mod_rast <- rasterize(mod_vect[mod_vect$year == year_sel], template_rast, field = 'IRCM') %>%
    subst(NA, 0)
  crs(mod_rast) <- crs(precip)
  
  for(j in 1:length(unique(wimas_wells$radius))){
    radius_sel <- unique(wimas_wells$radius)[j]
    
    well_buffer <- st_buffer(wells, radius_sel*1000)
    
    asdf <- extract(mod_rast, vect(well_buffer), 'table', exact = T) %>%
      pivot_longer(!ID, names_to = 'amt', values_to = 'frac') %>%
      mutate(amt = as.numeric(amt),
             amt_weight = amt*frac) %>%
      group_by(ID) %>%
      summarise(amt = sum(amt_weight, na.rm = T))
    # model_irr_m <- extract(mod_rast, vect(well_buffer), 'mean', na.rm = T, exact = T)$last/1000
    model_irr_m <- asdf$amt/1000
    model_irr_af <- model_irr_m*pi*((radius_sel*1000)^2)/1233

    wimas_irr_af <- filter(wimas_wells, year == year_sel & radius == radius_sel & use == 'IRR') %>%
      select(well, wimas = amt)
    
    comp_ij <- data.frame(
      well = wells$well,
      year = year_sel,
      radius = radius_sel,
      model = asdf$amt
    ) %>%
      left_join(wimas_irr_af)
    comp <- rbind(comp, comp_ij)
  }
}
write_csv(comp, 'data/crop_model/wimas_comparison.csv')
comp <- read_csv('data/dssat/wimas_comparison.csv') %>%
  filter(radius %in% 1:8)
ggplot(subset(comp, well == 'dg01' & radius %in% c(2,4,6)), aes(x = wimas, y = model, color = factor(radius))) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(vars(radius), scales = 'free', nrow=1) +
  xlab('WIMAS') +
  ylab('DSSAT') +
  scale_color_manual(values = c('black', 'black', 'black'), guide = 'none')
summary(lm(model~wimas, data = subset(comp, well == 'dg01' & radius == 6)))


# plot well buffers -------------------------------------------------------
year_sel <- 2008
radius_sel <- unique(wimas_wells$radius)[8]

well_buffer <- st_buffer(wells, radius_sel*1000)

mod_rast <- rasterize(mod_vect[mod_vect$year == year_sel], template_rast, field = 'IRCM') %>%
  subst(NA, 0)
crs(mod_rast) <- crs(precip)

ggplot() +
  geom_spatraster(data = mod_rast) +
  geom_sf(data = eksrb, fill = NA) +
  geom_sf(data = well_buffer, fill = NA, color = 'black') +
  scale_fill_viridis_c(na.value = NA)

model_irr_m <- extract(mod_rast, vect(well_buffer), 'mean', na.rm = T, exact = T)$last/1000
model_irr_af <- model_irr_m*pi*((radius_sel*1000)^2)/1233

wimas_irr_af <- filter(wimas_wells, year == year_sel & radius == radius_sel & use == 'IRR')$amt


# 4.5 ---------------------------------------------------------------------
mod45 <- read_csv('data/crop_model/summary_RCP45_NoAdapt.csv') %>%
  left_join(grid) %>%
  mutate(year = str_sub(as.character(HDAT), 1, 4),
         hyday = as.numeric(str_sub(as.character(HDAT), 5,7)),
         pyday = hyday - round(NDCH),
         hdate = ymd('1997-12-31') + days(hyday),
         pdate = ymd('1997-12-31') + days(pyday)) %>%
  st_as_sf(coords = c('centx', 'centy'), crs = 4269) %>% st_transform(5070)

template_rast <- rast(ext = ext(mod), res = res(precip))
mod45_vect <- vect(mod45)

mod45_summary <- data.frame(
  year = unique(mod45$year),
  model  = NA
)
for(i in 1:nrow(mod45_summary)){
  year_sel = mod45_summary$year[i]
  
  #set up raster from model output
  mod45_rast <- rasterize(mod45_vect[mod45_vect$year == year_sel], template_rast, field = 'IRCM')
  crs(mod45_rast) <- crs(precip)
  mod45_kraa <- mask(mod45_rast, kraa)
  #get irrigation total from raster
  cell_area_m2 <- res(mod45_kraa)[1] * res(mod45_kraa)[2]
  model_irr_m <- global(mod45_kraa, 'sum', na.rm = T)[1,1]/1000
  mod45_summary$model[i] <- cell_area_m2*model_irr_m/1233
}


full_ts <- rbind(select(comp, year, model), mod45_summary) %>%
  mutate(year = as.numeric(year),
         period = ifelse(year <= 2015, 'historical', 'projection'))

ggplot(full_ts, aes(x = year, y = model, color = period)) +
  geom_line()
ggplot(full_ts, aes(x = period, fill = period, y = model)) +
  geom_boxplot() +
  ylab('Irrigation amount [acre-ft]') +
  scale_x_discrete(labels = c('Historical (1991-2015)', 'Projection (2025-2099)')) +
  scale_fill_discrete(guide = NULL)



# plots -------------------------------------------------------------------
ggplot() +
  geom_spatraster(data = sum_precip) +
  geom_spatraster(data = mod_rast) +
  # geom_sf(data = mod) +
  geom_sf(data = eksrb, fill = NA) +
  geom_sf(data = kraa, fill = NA) +
  geom_sf(data = wells) +
  scale_fill_viridis_c(na.value = NA, name = 'precip')

#only gridmet
ggplot() +
  geom_spatraster(data = sum_precip) +
  geom_spatraster(data = mod_rast, fill = NA) +
  # geom_sf(data = mod) +
  geom_sf(data = eksrb, fill = NA) +
  geom_sf(data = kraa, fill = NA) +
  geom_sf(data = wells) +
  scale_fill_viridis_c(na.value = NA)

#only model output
ggplot() +
  geom_spatraster(data = sum_precip, fill = NA) +
  geom_spatraster(data = mod_rast) +
  # geom_sf(data = mod) +
  geom_sf(data = eksrb, fill = NA) +
  geom_sf(data = kraa, fill = NA) +
  geom_sf(data = subset(wells, well == 'dg01')) +
  scale_fill_viridis_c(na.value = NA)


