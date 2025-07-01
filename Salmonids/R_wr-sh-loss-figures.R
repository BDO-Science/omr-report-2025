library(tidyverse)
library(busdater)
library(janitor)
library(zoo)

#############################
#current WY loss data/figures
#############################

wy <- get_fy(Sys.Date(), opt_fy_start = '10-01')  #pull the water year based on BY designation in LTO docs
jpe <- 98893 #set natural winter-run JPE
jpe_hatch <- 135342 #set hatchery JPE

#pull in winter-run loss data
wrurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=',wy,
                '&species=1%3Aall&dnaOnly=no&age=no')
wr_loss <- read_csv(wrurl) %>%
  clean_names()
write.csv(wr_loss, 'Salmonids/output/wy_2025_wr_loss.csv', row.names = FALSE) #saving to include in data appendix

#pull in and summarize steelhead loss data
shurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year='
                ,wy,'&species=2%3Af&dnaOnly=no&age=no')
sh_import <- read_csv(shurl) %>%
  clean_names() 

write.csv(sh_import, 'Salmonids/output/wy_2025_sh_loss.csv', row.names = FALSE) #saving to include in data appendix

sh_loss <- sh_import %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  mutate(cumul = cumsum(loss)) %>%
  na.omit()

#summarize winter-run natural and hatchery loss data
wr_natural <- wr_loss %>% 
  filter(adipose_clip == 'Unclipped' &
           dna_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  mutate(cumul = cumsum(loss))

wr_hatchery <- wr_loss %>%
  filter(cwt_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(cumul = cumsum(loss))

#winter-run  weekly distributed loss
wr_thresholds <- read_csv('Salmonids/data/weeklyThresholds.csv') %>% #pulling in weekly distributed loss thresholds
  mutate(StartDate = dmy(paste0(StartDate,'-',wy))) %>% #converting to date format with current water year
  mutate(EndDate = dmy(paste0(EndDate,'-',wy))) %>% #ditto
  rowwise() %>%
  mutate(date = list(seq.Date(StartDate, EndDate, by = "day"))) %>%
  unnest(date) %>%
  select(date, HistoricPresent) %>%
  mutate(threshold = ((jpe*.005)*.5)*HistoricPresent)

wr_weekly <- data.frame(date = seq(as.Date('2024-12-01'), as.Date('2025-06-30'), 1)) %>%
  left_join(wr_natural, by = 'date') %>%
  left_join(wr_thresholds, by = 'date') %>%
  replace(is.na(.), 0) %>%
  mutate(threshold = round(threshold, 2)) %>%
  mutate(sum_7D_loss = rollsum(loss, k = 7, fill = NA, align = 'right')) %>%
  filter(date >= as.Date(paste0(wy,'-01-01')))

#steelhead weekly distributed loss
sh_weekly <- data.frame(date = seq(as.Date('2024-12-01'), as.Date('2025-06-30'), 1)) %>%
  left_join(sh_loss, by = 'date') %>%
  replace(is.na(.), 0) %>%
  mutate(threshold = 120) %>%
  mutate(sum_7D_loss = rollsum(loss, k = 7, fill = NA, align = 'right')) %>%
  filter(date >= as.Date(paste0(wy,'-01-01')))


###########################
#historical loss comparison
###########################

###genetic winter-run by month
CVP <- read_csv('Salmonids/data/CVP_genetics.csv')
SWP <- read_csv('Salmonids/data/SWP_genetics.csv')

wr_temp <- wr_loss %>% 
  filter(adipose_clip == 'Unclipped' &
           dna_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(n = sum(nfish)) %>%
  ungroup() %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(n = sum(n)) %>%
  mutate(class = 'current')

wr_by_month <- bind_rows(CVP, SWP) %>%
  mutate(month = month(SampleDateTime, label = TRUE),
         wy = get_fy(SampleDateTime, opt_fy_start = '07-01')) %>%
  filter(Genetic_Assignment == 'Winter') %>%
  group_by(month) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(class = 'historic') %>%
  bind_rows(wr_temp) %>%
  filter(month %in% c('Jan', 'Feb', 'Mar', 'Apr', 'May')) %>%
  group_by(class) %>%
  mutate(prop = prop.table(n))

###historic steelhead
sh_import_all_years <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Af&dnaOnly=no&age=no') %>%
  clean_names()

sh_by_month <- sh_import_all_years %>%
  mutate(date = as.Date(sample_time)) %>%
  mutate(class = if_else(date >= as.Date('2024-07-01'), 'current', 'historic'),
         month = month(date, label = TRUE),
         wy = get_fy(date, opt_fy_start = '07-01')) %>%
  filter(wy > 2008) %>%
  group_by(month, class) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  na.omit() %>%
  group_by(class) %>%
  mutate(prop = prop.table(loss)) %>%
  mutate(month = factor(month, levels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 
                                          'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 
                                          'May', 'Jun')))
