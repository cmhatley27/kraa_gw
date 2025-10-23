library(tidyverse)

well_info <- read_csv(file.path('data','wells','well_info.csv'))
gage_info <- read_csv(file.path('data','river','gage_info.csv'))

wells <- map(list.files(file.path('data','wells','clean'), full.names = TRUE), read_csv) %>%
  list_rbind(.)
rivs <- map(list.files(file.path('data','river','clean'), full.names = TRUE), read_csv) %>%
  list_rbind(.)

ftr <- read_csv(file.path('data', 'river','clean','Ftriley.csv'))
mhk <- read_csv(file.path('data', 'river','clean','Manhattan.csv'))
rl01 <- read_csv(file.path('data', 'wells','clean','rl01.csv'))


rl01_adj <- left_join(rl01, ftr) %>%
  left_join(., mhk, by = join_by(dateTime), suffix = c('_west', '_east')) %>%
  mutate(west_east_slope = (stage_west - stage_east)/(170.6 - 150.7),
         river_elev_interp = west_east_slope*(158.8 - 150.7) + stage_east)


join_and_interp <- function(well, west_gage, east_gage){
  well_dat <- filter(wells, id == well)
  west_dat <- filter(rivs, id == west_gage) %>%
    select(!id)
  east_dat <- filter(rivs, id == east_gage) %>%
    select(!id)
  
  rm_well <- well_info$river_mile[well_info$well == well]
  rm_west <- gage_info$river_mile[gage_info$location == west_gage]
  rm_east <- gage_info$river_mile[gage_info$location == east_gage]
  if(west_gage == 'Lawrence_upstream'){rm_west <- gage_info$river_mile[gage_info$location == 'Lawrence']}
  if(east_gage == 'Lawrence_upstream'){rm_east <- gage_info$river_mile[gage_info$location == 'Lawrence']}
  
  well_adj <- left_join(well_dat, west_dat) %>%
    left_join(., east_dat, by = join_by(dateTime), suffix = c('_west', '_east')) %>%
    mutate(west_east_slope = (stage_west - stage_east)/(rm_west - rm_east),
           stage_interp = west_east_slope*(rm_well - rm_east) + stage_east)
  
  return(well_adj)
}

rl01_adj <- join_and_interp('rl01','Ftriley','Manhattan')
write_csv(rl01_adj, file.path('data','well_river_interpolations.csv'))
ggplot(rl01_adj, aes(x = dateTime)) +
  geom_line(aes(y = gwl), color = 'black') +
  geom_line(aes(y = stage_west), color = 'red') +
  geom_line(aes(y = stage_east), color = 'blue')

dg01_adj <- join_and_interp('dg01','Lecompton','Lawrence')
ggplot(dg01_adj, aes(x = dateTime)) +
  geom_line(aes(y = gwl), color = 'black') +
  geom_line(aes(y = stage_west), color = 'red') +
  geom_line(aes(y = stage_east), color = 'blue') +
  geom_line(aes(y = stage_interp), color = 'green')

lv01_adj <- join_and_interp('lv01','Desoto','KC')
write_csv(lv01_adj, file.path('data','well_river_interpolations.csv'))
ggplot(lv01_adj, aes(x = dateTime)) +
  geom_line(aes(y = gwl), color = 'black') +
  geom_line(aes(y = stage_west), color = 'red') +
  geom_line(aes(y = stage_east), color = 'blue') +
  geom_line(aes(y = stage_interp), color = 'green')

jf02_adj <- join_and_interp('jf02','Lecompton','Lecompton')
write_csv(jf02_adj, file.path('data','well_river_interpolations.csv'))
ggplot(jf02_adj, aes(x = dateTime)) +
  geom_line(aes(y = gwl), color = 'black') +
  geom_line(aes(y = stage_west), color = 'red') +
  geom_line(aes(y = stage_east), color = 'blue') +
  geom_line(aes(y = stage_interp), color = 'green')


RL01_adj <- join_and_interp(RL01_level, Ftriley_gauge, Manhattan_gauges, 158.8,170.6,150.7)
RL02_adj  <- join_and_interp(RL02_level, Manhattan_gauges,Wamego_gauge, 137.75,150.7,128.5)
WB01_adj <- join_and_interp(WB01_level, Wamego_gauge, Nearbelvue_gauge, 126.8,128.5,116.9)
WB02_adj  <- join_and_interp(WB02_level, Nearbelvue_gauge, Topekawp_gauge, 112.8,116.9,87.0)
SN01_adj <- join_and_interp(SN01_level, Nearbelvue_gauge, Topekawp_gauge, 104.5,116.9,87.0)
SN02_adj  <- join_and_interp(SN02_level, Nearbelvue_gauge, Topekawp_gauge, 89.7,116.9,87.0)
JF01_adj <- join_and_interp(JF01_level, WestTopeka_gauge, Lecompton_gauge, 72.3,83.8,64.3)
JF02_adj <- join_and_interp(JF02_level, Lecompton_gauge, Lecompton_gauge, 64.4,64.3,64.3)
DG01_adj <- join_and_interp(DG01_level, Lecompton_gauge, Lawrenceup_gauge, 54.1,64.4,54.4)

GEMS4_1_adj  <- join_and_interp(GEMS4_1_level, Lecompton_gauge, Lawrenceup_gauge, 54.25,64.4,54.4)

DG02_adj <- join_and_interp(DG02_level, Lawrence_gauges, Desoto_gauges, 48.4,52.4,30.8)
DG03_adj  <- join_and_interp(DG03_level, Lecompton_gauge, Lawrenceup_gauge, 54.4,64.4,54.4)
LV01_adj <- join_and_interp(LV01_level, Desoto_gauges, Lakequivera_gauges, 30.6,30.8,14.8)
WY01_adj  <- join_and_interp(WY01_level, Lakequivera_gauge, Kansascity_gauges, 13.6,14.8,2)
PT01_adj <- join_and_interp(PT01_level, Wamego_gauge, Nearbelvue_gauge, 127.3,128.5,116.9)
PT02_adj  <- join_and_interp(PT02_level, Wamego_gauge, Nearbelvue_gauge, 127.3,128.5,116.9)
