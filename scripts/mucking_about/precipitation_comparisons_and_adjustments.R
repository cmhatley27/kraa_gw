library(tidyverse)
source('./scripts/functions/Theme+Settings.R')
source('./scripts/functions/data_helpers.R')


# Precipitation -----------------------------------------------------------
# Because some of the index wells are not located near precipitation gages
# with sufficiently long periods of record, we will likely have to rely
# on gridMET to fill in the gaps.

# Further, since we are using MACA climate projections which are based
# on gridMET, it may make sense to use gridMET data for precip inputs
# to all wells regardless of proximity to a precipitation gage.

# Either way we will need to investigate the relationship between gridMET
# precipitation and station-based precipitation.


### load data and compare station and gridmet precip ------------
# Some of the stations match up really well with the gridMET data at nearby
# wells (cor > 0.8), while others do not match up at all (cor < 0.3). Specifically,
# precipitation from airport gages (manhattan, topeka, and lawrence) have high
# correlations with nearby gridMET cells while precipitation from non-airport
# gages have low correlations.

station <- 'lawrence_airport'
well_sel <- 'dg01'
radius_sel <- 2

station_dat <- read_csv(paste0('./data/climate/met_station/clean/',station,'.csv')) %>%
  rename(station = precip)
gridmet_dat <- read_csv('./data/climate/gridmet/precip.csv') %>%
  mutate(date = date(date)) %>%
  filter(well == well_sel, radius == radius_sel) %>%
  select(date, gridmet = precip)

dat <- left_join(gridmet_dat, station_dat)

ggplot(subset(dat), aes(x = station, y = gridmet)) +
  geom_point() +
  geom_abline(slope = 1)
cor(dplyr::select(dat, !date), use = 'pairwise.complete')


### try different lead weightings to match up series ------------
# This may have to do with the way that precip is assigned to certain dates.
# PRISM (and thus gridMET, presumably) does not use local timezones but instead
# each day is defined as the 24 hours ending at noon GMT on a given date.
# Thus, in Kansas, precipitation recorded in gridMET for May 27th actually
# corresponds to the precipitation that fell between 6am on May 26th and 6am
# on May 27th.

# This seems to be confirmed when applying a weighted lag to the gridMET time
# series, where precipitation for each date is adjusted to be a weighted mean of
# the precipitation amounts for that date and the previous date. For all of the
# gage-gridMET combinations with low correlations, the optimal weight occurs at
# around 0.75, where 75% of the previous date's precip is added to 25% of the
# current date's (corresponding to the fact that precipitation for each date in 
# gridMET is actually comprised of roughly 75% of the previous date's precip)
lags <- tibble(
  weight = seq(0,1,by = 0.01),
  cor = NA
)
for (i in 1:nrow(lags)){
  w <- lags$weight[i]
  dat$gridmet_wlag <- (w*lag(dat$gridmet,1)) + ((1-w)*dat$gridmet)
  
  lags$cor[i] <- cor(dat$station, dat$gridmet_wlag, use = 'pairwise.complete')
}
ggplot(lags, aes(x = weight, y = cor)) +
  geom_line()


# bias correct lagged data ------------------------------------------------
# Using these adjusted precip series brings tthe correlations up to around 
# 0.7-0.8, which is where they are for the airport precip gages which don't
# need any adjustment
w <- 0.75
dat <- left_join(gridmet_dat, station_dat) %>%
  mutate(gridmet_wlag = (w*lag(gridmet,1, default = 0)) + ((1-w)*gridmet))

ggplot(subset(dat), aes(x = station, y = gridmet_wlag)) +
  geom_point() +
  geom_abline(slope = 1)
cor(select(dat, !date), use = 'pairwise.complete')


# compare monthly totals --------------------------------------------------
# Of course, all of this can be avoided if we aggregate precip to a monthly
# timestep instead.
dat_monthly <- dat %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(across(!date, sum))

ggplot(dat_monthly, aes(x = station, y = gridmet)) +
  geom_point() +
  geom_abline(slope = 1)
cor(select(dat_monthly, !c(year, month)), use = 'pairwise.complete')

