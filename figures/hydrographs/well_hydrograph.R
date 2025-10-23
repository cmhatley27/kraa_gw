# Load --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/theme.R')

wells <- map(list.files(file.path('data','wells','clean','daily'), full.names = T), read_csv) %>%
  list_rbind()

# Select well and plot ----------------------------------------------------
well_sel <- 'rl02'
trim_value <- 200

plot_dat <- wells %>%
  filter(well == well_sel & gwl >= trim_value)

ggplot(plot_dat, aes(x = date, y = gwl*3.28084)) +
  geom_point() +
  xlab('') +
  ylab('Well Head [ft]')

mean(plot_dat$gwl[plot_dat$type == 'index'], na.rm = T) - mean(plot_dat$gwl[plot_dat$type == 'prev'], na.rm = T)

# Save --------------------------------------------------------------------
ggsave(file.path('figures','hydrographs',paset0(well,'_hydrograph.png')),
       width = 6.5, height = 2, units = 'in')


# plot all wells ----------------------------------------------------------
ggplot(wells, aes(x = date, y = gwl, color = type)) +
  geom_point() +
  facet_wrap(vars(well), scales = 'free_y') +
  scale_color_manual(values = c('red','blue'), labels = c('Index Well', 'Previous Well'), name = NULL) +
  theme(legend.position = 'bottom')
ggsave('./figures/hydrographs/all_wells.png', height = 6, width = 12, units = 'in')

