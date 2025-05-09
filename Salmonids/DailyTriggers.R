library(tidyverse)
library(busdater)
jpe <- 234896
WY <- get_fy(Sys.Date())
thresholds <- data.frame(Date = seq(as.Date(paste0(WY,'-01-01')), as.Date(paste0(WY,'-05-31')), 1)) %>%
  mutate(New = case_when(Date >= as.Date(paste0(WY,'-05-01')) ~ 0,
                         Date >= as.Date(paste0(WY,'-04-01')) ~ .0000226,
                         Date >= as.Date(paste0(WY,'-03-01')) ~ .0000372,
                         Date >= as.Date(paste0(WY,'-02-01')) ~ .0000231,
                         Date >= as.Date(paste0(WY,'-01-01')) ~ .0000124)) %>%
  mutate(Old = case_when(Date >= as.Date(paste0(WY,'-05-01')) ~ 0,
                         Date >= as.Date(paste0(WY,'-04-01')) ~ .0000226,
                         Date >= as.Date(paste0(WY,'-03-01')) ~ .000146,
                         Date >= as.Date(paste0(WY,'-02-01')) ~ .0000991,
                         Date >= as.Date(paste0(WY,'-01-01')) ~ .0000635)) %>%
  mutate(SWP = New*jpe,
         CVP = Old*jpe)
                         

salvageurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year='
                     ,WY,'&species=1%3Af&dnaOnly=yes&age=no')

loss <- read.csv(salvageurl) %>%
  mutate(Date = as.Date(Sample.Time))

LAD <- loss %>% filter(LAD.Race == 'Winter') %>%
  group_by(Date) %>%
  summarize(Loss = sum(Loss)) %>%
  left_join(select(thresholds, Date, SWP, CVP), by = 'Date') %>%
  mutate(SWPtrigger = if_else(Loss > SWP, 'Y', 'N'),
         CVPtrigger = if_else(Loss > CVP, 'Y', 'N')) %>%
  gather(key = 'Project', value = 'Trigger', SWPtrigger, CVPtrigger) %>%
  filter(Trigger == 'Y') %>%
  group_by(Project) %>%
  summarize(n = n())

genetic <- loss %>% filter(DNA.Race == 'Winter') %>%
  group_by(Date) %>%
  summarize(Loss = sum(Loss)) %>%
  left_join(select(thresholds, Date, SWP, CVP), by = 'Date') %>%
  mutate(SWPtrigger = if_else(Loss > SWP, 'Y', 'N'),
         CVPtrigger = if_else(Loss > CVP, 'Y', 'N')) %>%
  gather(key = 'Project', value = 'Trigger', SWPtrigger, CVPtrigger) %>%
  filter(Trigger == 'Y') %>%
  group_by(Project) %>%
  summarize(n = n())

table <- loss %>% filter(DNA.Race == 'Winter') %>%
  group_by(Date) %>%
  summarize(Loss = sum(Loss)) %>%
  left_join(select(thresholds, Date, SWP, CVP), by = 'Date') %>%
  mutate(SWPtrigger = if_else(Loss > SWP, 'Y', 'N'),
         CVPtrigger = if_else(Loss > CVP, 'Y', 'N')) %>%
  rename('Genetic Winter-run Loss' = 'Loss',
         '2024 IOP Threshold' = 'SWP',
         '2023 IOP Threshold' = 'CVP',
         '2024 IOP Trigger' = 'SWPtrigger',
         '2023 IOP Trigger' = 'CVPtrigger')

write.csv(table, file = 'Salmonids/output/daily_triggers.csv', row.names = FALSE)

#######scrapping SacPAS surrogate stuff
library(rvest)
library(janitor)

hatcheryurl <- 'https://www.cbr.washington.edu/sacramento/data/delta_cwt_tables.html'
webpage <- read_html(hatcheryurl)
tables <- webpage %>%
  html_nodes("table")

surrogates <- html_table(tables[[6]]) %>% 
  row_to_names(row_number = 1) %>%
  filter(`Release Type` == 'Experimental') %>%
  select(1,3:12)

write.csv(surrogates, file = 'Salmonids/output/SRsurrogates.csv', row.names = FALSE)
