# Load --------------------------------------------------------------------
library(tidyverse)
library(zoo)
source(file.path('scripts','functions','Theme+Settings.R'))

wells <- map(list.files(file.path('data','wells','clean','15min'), full.names = T), read_csv) %>%
  list_rbind()


# Select well and input series --------------------------------------------
well <- 'lv01'
river_gage <- 'Desoto'
precip_gage <- 'Lawrence_Airport'

#load well for dateTimes
date_series <- wells %>%
  filter(id == well) %>%
  select(dateTime)

#load river
riv <- read_csv(file.path('data','river','clean',paste0(river_gage,'.csv'))) %>%
  select(!id)

#load precip and temp
precip_temp <- read_csv(file.path('data','precip',paste0(precip_gage,'.csv'))) %>%
  filter(year(DATE) >= 2018) %>%
  mutate(temp = ((TMAX + TMIN)/2)/10) %>%
  mutate(temp = na.approx(temp)) %>%
  select(dateTime = DATE, precip = PRCP, temp) %>%
  mutate(precip = ifelse(is.na(precip), 0 , precip),
         precip = precip/10)
#arbitrarily set precip to noon
hour(precip_temp$dateTime) <- 12

#generate pumping
pump <- tibble(dateTime = date_series$dateTime) %>%
  mutate(pumping = ifelse(month(dateTime) %in% c(5:8),1,0))

#combine
dat <- date_series %>%
  left_join(., riv) %>%
  left_join(., precip_temp) %>%
  left_join(., pump)
plot_dat <- dat %>%
  pivot_longer(!dateTime, names_to = 'series', values_to = 'value') %>%
  filter(!is.na(value))
plot_dat$series <- factor(plot_dat$series, levels = c('stage','precip','temp','pumping'),
                          labels = c('River Stage [ft]', 'Precipitation [mm]', 'Temperature [C]', 'Pumping [on/off]'))




# Define response function ------------------------------------------------
p3 <- function(x, A, a, n){
  theta <- A*((a^n)*(x^(n-1))*(exp(-1*a*x)))/gamma(n)
  return(theta)
}

gam <- function(x, A, n, a){
  theta <- A*((x^(n-1))*exp((-1*x)/a))/((a^n)*gamma(n))
  return(theta)
}

hantush <- function(x, A, a, b){
  theta <- A/(2*x*besselK(2*sqrt(b), 0))*exp((-1*x/a)-(a*b/x))
}

x <- seq(0,365*2, by = 0.1)
p3_test <- tibble(
  x = x,
  precip = gam(x,0.94,1.48,117.45),
  eto = gam(x,0.94,1.48,117.45)*-0.49,
  pumping = hantush(x,-0.00036, 589.76, 0.15)*1000
) %>%
  pivot_longer(!x, names_to = 'params', values_to = 'y') %>%
  # convert m to ft
  mutate(y = y*3.281)
p3_test$params <- factor(p3_test$params, levels = c('precip','eto','pumping'), labels = c('',' ','  '))

ggplot(p3_test) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_line(aes(x = x, y = y, color = params), linewidth = 1) +
  facet_wrap(vars(params), ncol = 1) +
  ylab('') +
  xlab('') +
  scale_color_discrete(guide = 'none') +
  theme(text = element_text(size = 12))

ggsave(file.path('figures','hydrographs','sn01_transfers.png'),
       width = 3, height = 6, units = 'in')

areas <- p3_test %>%
  group_by(params) %>%
  summarise(area = sum(y, na.rm = TRUE)*0.1)

p3_test$x[p3_test$y == max(p3_test$y, na.rm = T)]

# Run response function on input series -----------------------------------
p3_length <- 100
et_factor <- 0.7

dat_conv <- tibble(
  dateTime = precip$dateTime,
  p = precip$precip,
  t = precip$temp,
  c.p = convolve(p, p3(seq(0:p3_length),0.2,0.1,5), type = 'o', conj = F)[-(1:p3_length)],
  c.t = -1*convolve(t*et_factor, p3(seq(0:p3_length),0.2,0.1,5), type = 'o', conj = F)[-(1:p3_length)]
)

ggplot(dat_conv, aes(x = dateTime)) +
  #geom_col(aes(y = p), fill = 'black') +
  geom_line(aes(y = c.p), color = 'steelblue', linewidth = 1)

ggplot(dat_conv, aes(x = dateTime)) +
  #geom_line(aes(y = t), color = 'black') +
  geom_line(aes(y = c.t), color = 'maroon', linewidth = 1)

dat2 <- left_join(dat, select(dat_conv, dateTime, c.p, c.t)) %>%
  mutate(c.sum = c.p + c.t)

plot_dat2 <- pivot_longer(dat2, !dateTime, names_to = 'series', values_to = 'value') %>%
  filter(!is.na(value)) %>%
  filter(series %in% c('gwl','c.p','c.sum'))
plot_dat2$value[plot_dat2$series == 'c.p'] <- plot_dat2$value[plot_dat2$series == 'c.p'] + 
  min(plot_dat2$value[plot_dat2$series == 'gwl'])
plot_dat2$value[plot_dat2$series == 'c.sum'] <- plot_dat2$value[plot_dat2$series == 'c.sum'] + 
  min(plot_dat2$value[plot_dat2$series == 'gwl'])

ggplot(plot_dat2, aes(x = dateTime, y = value, color = series)) +
  geom_line() +
  scale_color_manual(limits = c('gwl','c.p','c.sum'), values = c('black', 'blue3', 'red'))


  
  
  

