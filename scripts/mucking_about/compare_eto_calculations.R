# libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(climateR)
library(AOI)
library(FAO56)
source('scripts/functions/utilities.R')

# test penman montieth ----------------------------------------------------
# This section compares 3 different estimates of ETo estimates for a single point 
# First, it grabs the gridMET meteorological data along with the built-in
# gridMET ETo estimate. Then, it calculates ETo using the FAO P-M equation
# as implemented in the FAO56 package. Last, it calculates ETo using the FAO
# P-M equation but from my own implementation. The last two chunks of code
# gather all of these estimates into a data frame and allow for plotting against
# one another.

point <- st_as_sf(read_csv('./data/wells/well_info.csv')[12,],
                  coords = c('long', 'lat'), crs = 4269)

dat <- getGridMET(AOI = point,
                  varname = c('pr','pet','tmmn','tmmx','vpd','srad','vs','rmin','rmax'),
                  startDate = '2000-01-01',
                  endDate = '2020-12-31')

eto_gridmet = dat$pet

#use package ETo calculation. Still have to input my own net radiation though
rad_conv = (dat$srad/1000000)*60*60*24
a = 0.23
rad_ns = (1-a)*rad_conv
tminC = dat$tmmn-273.15
tdew = tmin - ((100-dat$rmax)/5)
tmaxC = dat$tmmx-273.15
ea = 0.6108*exp((17.27*tmin)/(tmin+273.3))
#calculate net longwave radiation
sigma = 4.903*10^-9
rad_nl = sigma*(((tminC+273.15)^4+(tmaxC+273.15)^4)/2)*(0.34-0.14*sqrt(ea))
#calculate net radiation (incoming solar - outgoing longwave)
rad_net = rad_ns - rad_nl

eto_fao <- ETo_FPM(T_min = dat$tmmn-273.15, T_max = dat$tmmx-273.15, u_2 = dat$vs, R_n = rad_net, elev = 264)

#custom ETo function that SHOULD BE EXACTLY THE SAME AS FAO BUT NOT FOR SOME REASON
pm <- function(tmin, tmax, rad, vs, elev){
  #convert temp to C
  tminC = tmin - 273.15
  tmaxC = tmax - 273.15
  #calculated saturated vapor pressure at tmin and tmax
  satminC = 0.6108*exp(17.27*tminC/(tminC+273.3))
  satmaxC = 0.6108*exp(17.27*tmaxC/(tmaxC+273.3))
  es = (satminC + satmaxC)/2
  #cacluate actual vapor pressure
  #ea = (satminC*(rmax/100) + satmaxC*(rmin/100))/2
  ea = satminC
  #vpd = es-ea
  # ea = es - vpd
  #convert W to MJ/d
  rad_conv = (rad/1000000)*60*60*24
  #calculate net solar radiation from albedo
  a = 0.23
  rad_ns = (1-a)*rad_conv
  #calculate net longwave radiation
  sigma = 4.903*10^-9
  rad_nl = sigma*(((tminC+273.15)^4+(tmaxC+273.15)^4)/2)*(0.34-0.14*sqrt(ea))
  #calculate net radiation (incoming solar - outgoing longwave)
  rad_net = rad_ns - rad_nl
  
  #get mean temp
  t = ((tminC + tmaxC)/2)
  #calculate slope of vapour pressure curve using mean temp
  d = 4098*(0.6108*exp((17.27*t)/(t + 237.3)))/((t+237.3)^2)
  #psychrometric constant
  p = 101.3 * ((293 - 0.0065 * elev)/293)^5.26
  # g = 0.665*10^-3 * p
  g = (1.013 * 10^(-3) * p)/(0.622 * 2.45)
  #calculate eto
  et1 = (0.408*d*rad_net) + (g*(900/(t+273))*vs*(es - ea))
  et2 = d+g*(1+(0.34*vs))
  return(et1/et2)
}
eto_me <- pm(tmin = dat$tmmn, tmax = dat$tmmx, rad = dat$srad, vs = dat$vs, elev = 264)

#compare
ets <- tibble(
  gridmet = eto_gridmet,
  fao = eto_fao,
  me = eto_me
)
ggplot(ets, aes(x = eto_fao, y = eto_me)) +
  geom_point() +
  geom_abline(slope = 1)

