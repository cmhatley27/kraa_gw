# calculate stats on calibration/validation -------------------------------
library(tidyverse)
source('./scripts/functions/Theme+Settings.R')

kge <- function(x,y, modified = T){
  r = cor(x,y,use = 'pairwise.complete')
  ux = mean(x, na.rm = T)
  uy = mean(y, na.rm = T)
  sx = sqrt(var(x, na.rm = T))
  sy = sqrt(var(y, na.rm = T))
  
  b = uy/ux
  g = sy/sx
  if(modified == T){g = (sy/uy)/(sx/ux)}
  
  t1 = (r-1)^2
  t2 = (b-1)^2
  t3 = (g-1)^2
  kge = 1-sqrt(t1+t2+t3)
  return(kge)
}

gwl <- read_csv('./data/model/dg01/gwl.csv') %>%
  mutate(date = date + months(1) - days(1))

out <- read_csv('./data/model/dg01/output85.csv') %>%
  select(date = 1, obs = 2, pred = 3, resid = 4, noise = 5, precip = 6, eta = 7, irr = 8)

calibration_period <- seq.Date(ymd('1982-01-01'), ymd('2014-12-31'), by = '1 day')
validation_period <- seq.Date(ymd('2015-01-01'), ymd('2023-12-31'), by = '1 day')

out <- out %>%
  mutate(set = ifelse(date %in% calibration_period, 'cal',
                      ifelse(date %in% validation_period, 'val', 'proj')))

show_proj <- T
if(show_proj == T){
  ggplot(out, aes(x = date)) +
    geom_point(aes(y = obs)) +
    geom_line(aes(y = pred, color = set)) +
    scale_color_manual(values = c('red2','green4','steelblue'), labels = c('Calibration','Projection','Validation'),
                       name = NULL) +
    labs(x = NULL, y = 'gwl [m]') +
    theme(legend.position = 'bottom')
}else {
  ggplot(subset(out, set != 'proj'), aes(x = date)) +
    geom_point(aes(y = obs)) +
    geom_line(aes(y = pred, color = set)) +
    scale_color_manual(values = c('red2', 'steelblue'), labels = c('Calibration','Validation'),
                       name = NULL) +
    labs(x = NULL, y = 'gwl [m]') +
    theme(legend.position = 'bottom')
  # ggsave('./figures/hydrographs/dg01_cal_val.png', height = 5, width = 10, units = 'in')
}


out %>% filter(!is.na(obs)) %>%
  group_by(set) %>%
  summarise(cor = cor(pred, obs, use = 'pairwise.complete'),
            r2 = cor(pred, obs, use = 'pairwise.complete')^2,
            rmse = sqrt(mean((obs-pred)^2, na.rm = T)),
            kge = kge(obs, pred))

# trends and aggregation --------------------
library(trend)
mk.test(out$pred[out$set == 'proj'])

monthly <- out %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(pred = mean(pred, na.rm = T),
            obs = mean(obs, na.rm = T)) %>%
  mutate(date = ym(paste(year,month, sep = '-')),
         projection = ifelse(date %in% c(calibration_period, validation_period), F, T))
ggplot(monthly, aes(x = date)) +
  geom_point(aes(y = obs)) +
  geom_line(aes(y = pred, color = projection)) +
  scale_color_manual(values = c('red2', 'green4'))

mk.test(monthly$pred[year(monthly$date) >= 2024])

yearly <- out %>%
  group_by(year = year(date)) %>%
  summarise(pred = mean(pred, na.rm = T),
            obs = mean(obs, na.rm = T)) %>%
  mutate(projection = ifelse(year >= year(validation_period[length(validation_period)]), T, F))
ggplot(yearly, aes(x = year)) +
  geom_point(aes(y = obs)) +
  geom_line(aes(y = pred, color = projection)) +
  scale_color_manual(values = c('red2', 'green4'), labels = c('Historical', 'Projection'), name = NULL) +
  labs(x = NULL, y = 'gwl [m]') +
  theme(legend.position = 'bottom') +
  geom_smooth(data = subset(yearly, projection == T), aes(x = year, y = pred), method = 'lm', se = F, color = 'green4')
ggsave('./figures/hydrographs/dg01_proj.png', height = 5, width = 10, units = 'in')
mk.test(yearly$pred[yearly$projection == T])


# individual series -------------------------------------------------------
out45 <- read_csv('./data/model/dg01/output45.csv') %>%
  select(date = 1, obs = 2, pred = 3, resid = 4, noise = 5, precip = 6, eta = 7, irr = 8) %>%
  mutate(scenario = 'rcp45')
out85 <- read_csv('./data/model/dg01/output85.csv') %>%
  select(date = 1, obs = 2, pred = 3, resid = 4, noise = 5, precip = 6, eta = 7, irr = 8) %>%
  mutate(scenario = 'rcp85')
out_comp <- rbind(out45, out85)


ggplot(out_comp, aes(x = date, y = irr, color = scenario)) +
  geom_line()
ggplot(out_comp, aes(x = date, y = precip+eta, color = scenario)) +
  geom_line()



