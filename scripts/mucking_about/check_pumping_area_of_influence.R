# data --------------------------------------------------------------------
library(tidyverse)

pumping_annual <- read_csv('./data/pumping/wimas/annual.csv')

well_sel <- 'rl02'
gwl <- read_csv(paste0('./data/wells/clean/daily/',well_sel,'.csv'))

#aggregate gwl to annual mean
gwl_annual <- gwl %>%
  group_by(year = year(date)) %>%
  summarise(gwl = mean(gwl, na.rm = T))

#get total pumping per year around selected well for each major use
pumping_well <- pumping_annual %>%
  filter(well == well_sel)


# pumping by use through time ---------------------------------------------
ggplot(subset(pumping_well, year >= 1981), aes(x = year, y = amt, fill = use)) +
  geom_area(color = 'black') +
  facet_wrap(vars(radius), scales = 'free_y')


# total IRR pumping by radius for select years --------------------------------
years_sel <- c(1981, 1995, 2010, 2024)

pumping_years <- filter(pumping_well, year %in% years_sel & use == 'IRR')

ggplot(pumping_years, aes(x = radius, y = amt, color = factor(year))) +
  geom_point() + geom_line()

# IRR pumping correlations with gwl -------------------------------------------
#join annual groundwater levels and pumping amounts
#filter to period of interest. Pumping data only reliable >= 1981
pumping_gwl <- filter(pumping_well, use %in% c('IRR')) %>%
  left_join(gwl_annual) %>%
  filter(year >= 1981)

#plot gwl vs pumping amount for different radii for each use
ggplot(pumping_gwl, aes(x = amt, y = gwl, color = use)) +
  geom_point(color = 'green4') +
  geom_smooth(method = 'lm', se = F, color = 'green3') +
  facet_wrap(vars(radius), scale = 'free_x') +
  labs(x = 'Annual Irrigation Pumping [acre-ft]', y = 'Mean Groundwater Level [m]')

#calculate correlations
cors <- filter(pumping_gwl) %>%
  group_by(use, radius) %>%
  summarise(cor = cor(gwl, amt, use = 'pairwise.complete'))
cors
ggplot(cors, aes(x = radius, y = cor)) +
  geom_point() + geom_line()
