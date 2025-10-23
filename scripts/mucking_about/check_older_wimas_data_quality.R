library(tidyverse)
# check pumping data from different periods -------------------------------
wimas <- read_csv('./data/pumping/wimas.csv')
all_use <- wimas %>%
  filter(use %in% c('IRR','IND','MUN')) %>%
  group_by(year, use) %>%
  summarise(amt = sum(amt),
            n = n())

# total reported use across the 3 main categories. Big change in 1981,
# but no detectable jump in 1990 when use data started undergoing quality control
# Thus, perhaps pumping data from 1981-present is fine to use.
ggplot(all_use, aes(x = year, y = amt, fill = use)) +
  geom_area(color = 'black') +
  geom_vline(xintercept = 1990, linetype = 'dashed') +
  labs(x = NULL, y = 'Water Use [m^3]')

# number of points of diversion across the 3 main categories
ggplot(all_use, aes(x = year, y = n, fill = use)) +
  geom_area(color = 'black') +
  geom_vline(xintercept = 1990, linetype = 'dashed') +
  labs(x = NULL, y = '# points of diversion')

# trim to 1981 and look at linear trends in use.
use_1981 <- filter(all_use, year >= 1981)
# irrigation and municipal use steadily increase while industrial use drops
# off sharply. No seeming artifacts introduced by changes in how water rights were
# administered and monitored during this period
ggplot(use_1981, aes(x = year, y = amt, color = use)) +
  geom_line() +
  geom_smooth(linetype = 'dashed', se = F, method = 'lm') +
  ylab('Water Use [m^3]')
