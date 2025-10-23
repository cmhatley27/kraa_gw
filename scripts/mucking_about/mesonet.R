meso <- read_delim('data/climate/mesonet/rossville.txt') %>%
  rename(date = 1, station = 2, precip = 3, eto = 4, etr = 5) %>%
  mutate(eto = as.numeric(eto))

meso %>% group_by(year = year(date)) %>%
  summarise(eto = sum(eto, na.rm = T))
