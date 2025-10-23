library(tidyverse)
library(sf)
library(terra)
source('./scripts/functions/theme.R')

# Is ETa sensitive to distance from well? --------------------------------
# not really.
eta <- read_csv('./data/climate/openet/eta.csv')

eta_global <- eta %>%
  group_by(well, radius) %>%
  summarise(eta = mean(eta, na.rm = T))
# Mean ETa is roughly equal to the global mean of the entire basin at radii >= 2 km.
# At radii < 2 km ETa amounts are slightly more variable from well to well.
ggplot(eta_global, aes(x = radius, y = eta)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(well))

eta_month <- eta %>%
  group_by(well, month = month(date), radius) %>%
  summarise(eta = mean(eta, na.rm = T))
# Summer months show the most variability across radii
well_sel <- 'sn01'
ggplot(subset(eta_month, well == well_sel), aes(x = radius, y = eta)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(month)) +
  ggtitle(well_sel)

# does gridmet ETo change with distance from well? ------------------------
# No real observable differences, likely because of the very coarse resolution
# of the input rasters (4km). The max radius here only clips a maximum
# of 16 grid cells.
etr <- read_csv('./data/climate/gridmet/etr.csv')

etr_global <- etr %>%
  group_by(well, radius) %>%
  summarise(etr = mean(etr, na.rm = T))
ggplot(etr_global, aes(x = radius, y = etr)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(well))

etr_month <- etr %>%
  group_by(well, month = month(date), radius) %>%
  summarise(etr = mean(etr, na.rm = T))
well_sel <- 'dg01'
ggplot(subset(etr_month, well == well_sel), aes(x = radius, y = etr)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(month)) +
  ggtitle(well_sel)


# Adjust ETr using crop coefficient ---------------------------------------
eta <- read_csv('./data/climate/openet/eta.csv') %>%
  mutate(year = year(date), month = month(date))
eto <- read_csv('./data/climate/gridmet/eto.csv') %>%
  mutate(year = year(date), month = month(date))

et_comb <- left_join(eto, select(eta, !date))

well_sel <- 'sn01'
rad_sel <- 2

et_fil <- filter(et_comb, well == well_sel & radius == rad_sel)

et_month <- et_fil %>%
  group_by(year, month) %>%
  summarise(eto = sum(eto),
            eta = mean(eta)) %>%
  mutate(kc = eta/eto,
         date = ym(paste(year,month,sep = '-')))

#plot eto vs eta
ggplot(subset(et_month, year %in% 2017:2019), aes(x = date)) +
  geom_line(aes(y = eto)) +
  geom_line(aes(y = eta), color = 'red') +
  labs(x = NULL, y = 'Monthly ET [mm]')

kc <- group_by(et_month, month) %>%
  summarise(kc = mean(kc, na.rm = T))

ggplot(kc, aes(x = factor(month), y = kc)) +
  geom_line(group = 1) + geom_point() +
  labs(x = 'Month', y = 'Crop Coefficient')
# compare adjusted daily eto values to monthly eta
et_kc <- left_join(et_fil, kc) %>%
  mutate(eta_est = eto*kc)

ggplot(subset(et_kc, date >= '2010-01-01'), aes(x = date, y = eta_est)) +
  geom_point() +
  geom_point(aes(y = eta/30), color = 'red')

ggplot(et_kc, aes(x = eta/30, y = eta_est)) +
  geom_point() +
  geom_abline(slope  = 1)

# aggregate to monthly
et_month_kc <- et_kc %>%
  group_by(year, month) %>%
  summarise(eta = mean(eta),
            eta_est = sum(eta_est)) %>%
  mutate(date = ymd(paste(year,month,'01', sep = '-')))

ggplot(subset(et_month_kc, year %in% 1995:2005), aes(x = date, y = eta_est)) +
  geom_line() +
  geom_line(aes(y = eta), color = 'red') +
  labs(x = NULL, y = 'Monthly ET [mm]')

ggplot(et_month_kc, aes(x = eta, y = eta_est)) +
  geom_point() +
  geom_abline(slope  = 1)

mean((et_month %>% group_by(year) %>% summarise(eto = sum(eto)))$eto)
mean((et_month_kc %>% group_by(year) %>% summarise(eta = sum(eta_est)))$eta)


#quantile mapping as alternate bias correction approach. crop factor scaling
#is simpler and produced better (though similar) results.
# library(qmap)
# dat_fit <- filter(dat, year(date) >= 2020)
# fq <- fitQmap(dat$station, dat$gridmet_wlag, method = 'PTF', wet.day = T)
# dq <- doQmap(dat$gridmet_wlag, fq)
# 
# dat$gridmet_qmap <- dq
# 
# ggplot(dat, aes(x = station, y = gridmet_qmap)) +
#   geom_point() +
#   geom_abline(slope = 1)
