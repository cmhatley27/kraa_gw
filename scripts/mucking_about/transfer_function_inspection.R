library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/theme.R')

well_sel <- 'dg01'
model_name <- c('flex_exp')

model_dir <- paste0('data/models/',well_sel,'/',model_name,'/')
input_dir <- paste0(model_dir,'in/')
output_dir <- paste0(model_dir,'out/')

block <- read_csv(paste0(output_dir[1],'rch_block.csv')) %>%
  rename(day = 1, r = 2) %>%
  mutate(r_sum = cumsum(r))
step <- read_csv(paste0(output_dir[1],'rch_step.csv')) %>%
  rename(day = 1, r = 2)

ggplot(data = block, aes(x = day, y = r)) + geom_line()
ggplot(data = step, aes(x = day, y = r)) + geom_line()
