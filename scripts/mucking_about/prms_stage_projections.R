library(tidyverse)
library(terra)
library(sf)
source('./scripts/functions/Theme+Settings.R')
source('./scripts/functions/data_helpers.R')

# Subset downloaded model outputs to study area -----------------------------
# PRMS model outputs are VERY large (~5 GB compressed for each individual GCM 
# and RCP run), so here I am just taking the raw downloaded and subsetting them
# to the EKSRB for further processing.

# Get stream segments in study area. Segments shapefile retrievevd from
# https://www.sciencebase.gov/catalog/item/6373c4bdd34ed907bf6c6e4d
segs <- st_read('./data/river/prms/Segments_subset.shp')
eksrb <- st_read('./data/shapefiles/eksrb/watershed_bndry.shp') %>%
  st_transform(., st_crs(segs))
segs_subset <- st_intersection(segs, eksrb)

# create vector of the segment IDs for the segments in the study area
cols_sel <- as.character(segs_subset$seg_id_nat)

# list all raw downloaded model outputs for each GCM and RCP. Retrieved manually
# from https://www.sciencebase.gov/catalog/item/63890125d34ed907bf78e97f. Note
# that I have currently just downloaded the outputs that use 'dynamic' land cover
# and I'm not really sure how they implemented that/what effect it has on the 
# output. There are separate model outputs that can be downloaded that use
# static land cover.
files <- list.files('./data/river/prms/raw/')
file_paths <- paste0('./data/river/prms/raw/',files)

for(i in 1:length(files)){
  #loop through each file in the raw downloads folder
  file_name <- files[i]
  file_path <- file_paths[i]
  
  gcm <- word(file_name, 2, sep = '_')
  scenario <- word(file_name, 3, sep = '_')
  
  #create save path based on the GCM and RCP scenario
  save_dir <- paste0('./data/river/prms/subset/',gcm,'_',scenario,'.csv')
  
  #skip those that have already been subset
  if(file.exists(save_dir)) next
  
  # read in the raw model output and select only the stream segments in the
  # study area. This takes a very long time because it has to decompress the 
  # raw model output (.csv.gz), which is ~5 GB compressed and ~20 GB decompressed.
  # Using fread here makes things a bit faster and allows subsetting directily
  # in the function parameters.
  subset <- fread(file_path, select = cols_sel)
  write_csv(subset, save_dir)
}


# check out projections ---------------------------------------------------
segs <- st_read('./data/river/prms/Segments_subset.shp')
eksrb <- st_read('./data/shapefiles/eksrb/watershed_bndry.shp') %>%
  st_transform(., st_crs(segs))
segs_subset <- st_intersection(segs, eksrb)

gages <- tibble(
  x = c(-94.9646893),
  y = c(38.9833375)
) %>%
  st_as_sf(., coords = c('x','y'), crs = 4269) %>%
  st_transform(., st_crs(segs_subset))

seg_gage <- as.character(segs_subset[st_nearest_feature(gages, segs_subset),]$seg_id_nat)

ccsm_hist <- read_csv('./data/river/prms/subset/CCSM4_historical.csv') %>%
  select(Date, all_of(seg_gage)) %>%
  rename(date = 1, q = 2) %>%
  mutate(q = q*0.028316847,
         id = 'ccsm_hist')
ccsm_45 <- read_csv('./data/river/prms/subset/CCSM4_rcp45.csv') %>%
  select(Date, all_of(seg_gage)) %>%
  rename(date = 1, q = 2) %>%
  mutate(q = q*0.028316847,
         id = 'ccsm_rcp45')
desoto <- read_csv('./data/river/gages/clean/Desoto.csv') %>%
  select(date, q, id)

dat <- rbind(desoto, ccsm_hist, ccsm_45)

ggplot(subset(dat, year(date) %in% 1997), aes(x = date, y = q, color = id)) +
  geom_line()

dat_wide_monthly <- dat %>%
  group_by(year = year(date), month = month(date), id) %>%
  summarise(q = mean(q)) %>%
  mutate(date = ym(paste(year, month))) %>%
  select(date, id, q) %>%
  pivot_wider(id_cols = date, names_from = id, values_from = q)

ggplot(subset(dat_wide_monthly, !is.na(Desoto) & !is.na(ccsm_rcp45)), aes(x = Desoto, y = ccsm_rcp45)) +
  geom_point() +
  geom_abline(slope = 1)
