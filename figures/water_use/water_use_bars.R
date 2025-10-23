library(tidyverse)
library(sf)
source('scripts/functions/Theme+Settings.R')

well_info <- read_csv(file.path('data','wells','well_info.csv')) %>%
  st_as_sf(., coords = c('long','lat'), crs = 'epsg:4269') 

wimas <- read_csv(file.path('data','WIMAS','ks_water_use_1990_2022.csv')) %>%
  st_as_sf(., coords = c('long_nad83','lat_nad83'), crs = 'epsg:4269') 

for(well in well_info$well){
well_select <- well
index_well <- filter(well_info, well == well_select)
gwl <- read_csv(file.path('data','wells','clean','15min', paste0(well_select,'.csv')))
avg_gw <- gwl %>%
  group_by(year = year(dateTime)) %>%
  summarise(gwl = mean(gwl, na.rm = TRUE))

distance_threshold <- 4

selected_wells <- wimas %>%
  mutate(distance = as.numeric(st_distance(index_well, .))) %>%
  filter(distance <= distance_threshold*1000) %>%
  filter(avg_2013_2022 > 0)

use <- selected_wells %>%
  select(contains(c('_IRR_','_MUN_','_IND_','_REC_','PDIV_ID_1'))) %>%
  st_drop_geometry() %>%
  pivot_longer(., !PDIV_ID_1, names_to = 'var', values_to = 'val') %>%
  mutate(use = word(var, 3, sep = '_'),
         year = as.numeric(word(var, 4, sep = '_'))) %>%
  select(!var) %>%
  pivot_wider(., id_cols = c(PDIV_ID_1, year), names_from = use, values_from = val) %>%
  group_by(year) %>%
  summarise(across(c(IRR, MUN, IND, REC), ~sum(.x)*43560*(0.3048^3))) %>%
  pivot_longer(!year, names_to = 'use', values_to = 'val')

plot <- ggplot(use, aes(x = year, y = val, fill = use)) +
  geom_col() +
  xlab('') +
  ylab('') +
  ggtitle(paste0('Water use [m^3/yr] from wells within ',distance_threshold,' km of well ',well_select))

ggsave(file.path('figures','water_use',paste0(well,'_use_bars.png')), plot, height = 4, width = 20/3, units = 'in')
}
