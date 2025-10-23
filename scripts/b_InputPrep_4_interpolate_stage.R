# data --------------------------------------------------------------------
library(tidyverse)
riv_info <- read_csv('data/river/gages/gage_info.csv')
gage_sel <- 'Lecompton'

# go to: https://waterwatch.usgs.gov/?m=mkrc&sno=07050500
# and enter gage id for selected gage to get an image of the rating curve.
# This script will only work for rating curves that are a square root function.
# Look for break points in the curve where it transitions between functions.
# For the Lecompton gage, there is a point around 140,000 cfs where it transitions
# from a square root relationship to something slightly more linear. Here, we are
# fitting the rating curve to just the square root section and checking the interpolated
# period to make sure that the discharge values are within the range of this section.

riv <- read_csv(paste0('data/river/gages/clean/',gage_sel,'.csv')) %>%
  #trim outlier observations that are clearly errors
  mutate(stage = ifelse((stage-mean(stage, na.rm = T))/sqrt(var(stage, na.rm = T)) > 5, NA, stage))

#discharge value in cfs where the rating curve breakpoint occurs. Before this point
#the relationship should be square root, after this it might be more linear
curve_bp <- 140000

# select data for rating curve --------------------------------------------
ggplot(subset(riv, year(date) %in% 1980:2024), aes(x = date)) +
  geom_line(aes(y = q/100)) +
  geom_line(aes(y = stage), color = 'red')

#select year before which we interpolate stages
interp_year <- 1990

#select year to use for rating curve
years_sel <- 1993

curve_dat <- filter(riv, year(date) %in% years_sel) %>%
  filter(!is.na(stage)) %>%
  filter(q*35.3 <= curve_bp)

ggplot(curve_dat, aes(x = q, y = stage, color = factor(year(date)))) +
  geom_point()


# fit curve and interpolate missing stage values --------------------------
curve_mod <- lm(stage^2~q + 0, data = curve_dat)
summary(curve_mod)

ggplot(curve_dat, aes(x = q, y = stage)) +
  geom_point() +
  geom_point(aes(y = sqrt(curve_mod$coefficients[1]*q)), color = 'red')

riv <- mutate(riv, stage = ifelse(year(date) < interp_year, sqrt(curve_mod$coefficients[1]*q), stage))

ggplot(subset(riv, year(date) %in% 1980:2024), aes(x = date)) +
  geom_line(aes(y = q/100)) +
  geom_line(aes(y = stage), color = 'red')

# save --------------------------------------------------------------------
write_csv(riv, paste0('data/river/gages/clean/stage_adj/',gage_sel,'.csv'))

