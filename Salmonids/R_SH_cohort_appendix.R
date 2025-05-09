library(tidyverse)
library(busdater)
library(ggridges)
library(viridis)

#################### RST summary
query <- expand.grid(site = c('TIS', 'KNL', 'LFR', 'LSR', 'LAR'), year = seq(2009,2023,1))
rstList <- list()

for(i in 1:nrow(query)){
  site = query$site[i]
  year = query$year[i]
  index = paste0(site,'_',year)
  url <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year=', year, '&species=RBT%3ANA&loc=trap%3A', site, '%3A0&typeData=raw')
  
  temp <- read.csv(url) %>% mutate(site = site) %>%
    rename('Catch' = 2)
  
  if (ncol(temp) >= 3) {
    rstList[[index]] <- temp
  }
}

data <- bind_rows(rstList) %>% 
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Date)) %>%
  mutate(WY = get_fy(Date, opt_fy_start = '10-01'))

summary <- data %>% group_by(WY) %>%
  summarize(`Raw Catch` = sum(Catch, na.rm = TRUE), `Active Traps` = n_distinct(site, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`CPUE (fish/trap)` = `Raw Catch`/`Active Traps`) %>%
  filter(`CPUE (fish/trap)` > 0) %>%
  gather(key = 'Method', value = 'Catch',2:4) %>%
  mutate(Method = factor(Method, levels = c('CPUE (fish/trap)', 'Active Traps', 'Raw Catch')))

graph <- ggplot(summary) +
  geom_col(aes(x = factor(WY), y = Catch, fill = Method), position = 'dodge', color = 'black') + 
  labs(x = 'Water Year', y = 'Count', fill = '') +
  facet_wrap(~Method, ncol = 1, scales = 'free_y') + 
  scale_fill_viridis(discrete=TRUE, option="cividis") +
  theme_bw() +
  theme(plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        legend.position = 'bottom')
graph

ggsave(graph, file = 'Salmonids/appendix_outputs/RSTgraph.png', units = 'in', height = 7.5, width = 6.5)

#################### Annual steelhead cumulative loss by period and water year type

#Read in data dependencies
loss <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Af&dnaOnly=no&age=no')
waterDay <- readRDS('Salmonids/data/waterDay.rds')
wytype <- read.csv('Salmonids/data/WYtype.csv')

#Process loss data to find cumulative loss by year, biop, and water year type
cumulative <- loss %>% 
  select(Date = 1, 2, 9, 10, ExpSalv = 11, 12) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(WY = get_fy(Date, opt_fy_start = '10-01')) %>%
  mutate(wDay = waterDay(Date)) %>% 
  group_by(WY) %>%
  mutate(cumuloss = cumsum(Loss)) %>%
  mutate(Status = case_when(WY < 2009 ~ 'Pre-2009 BiOp',
                            WY > 2008  ~ '2009 & 2019 BiOps')) %>%
  left_join(wytype, by = 'WY') %>%
  filter(!is.na(Date)) %>%
  mutate(History = if_else(WY == '2024', 'Current', 'Past')) %>%
  mutate(Type2 = if_else(TYPE %in% c('W', 'AN'), 'Wet', 'Dry')) %>%
  mutate(Status = factor(Status, levels = c('Pre-2009 BiOp', '2009 & 2019 BiOps')))

#summarize additional data for graphing
years <- cumulative %>% 
  group_by(WY, Status, TYPE, Type2) %>% 
  summarize(Day = max(wDay), Loss = max(cumuloss))
max2024 <- cumulative %>% 
  filter(WY == 2024 & cumuloss == max(cumuloss)) %>% 
  pull(cumuloss)
loss2024 <- cumulative %>% 
  filter(WY == 2024) %>% 
  select(8,9)

#Create Plot
CumulLoss <- ggplot() +
  geom_line(filter(cumulative, WY < 2024), 
            mapping = aes(x = wDay, y = cumuloss, group = factor(WY)), color = 'grey', linewidth = 1)+
  geom_line(loss2024,
            mapping = aes(x = wDay, y = cumuloss), color = 'steelblue2', linewidth = 1.5) +
  geom_text(filter(years, Loss >= max2024 & WY < 2024), mapping = aes(x = Day+6, y = Loss, label = WY), 
            size = 2.5, fontface = 'bold') +
  facet_grid(Type2 ~ factor(Status, levels = c('Pre-2009 BiOp', '2009 & 2019 BiOps'))) +
  theme_bw() + 
  theme(legend.position = 'none',
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        strip.text = element_text(face = 'bold.italic'),
        axis.title = element_text(size = 15)) +
  labs(x = 'Date', y = 'Cumulative Loss', title = 'Current and Historic Steelhead Cumulative Loss') +
  scale_x_continuous(breaks = c(0,100,200,300), labels = c('Oct 1', 'Jan 8', 'Apr 18', 'July 26')) +
  scale_y_continuous(breaks = seq(0,15000,2500))
CumulLoss

ggsave(CumulLoss, file = 'Salmonids/appendix_outputs/cumuLoss.png')

#################### Trawl data summary
query <- expand.grid(site = c('SR055', 'SJ054', 'SB018'), year = seq(2009,2023,1))
trawlList <- list()

for(i in 1:nrow(query)){
  site = query$site[i]
  year = query$year[i]
  index = paste0(site,'_',year)
  url <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year='
                ,year,'&species=RBT%3ANA&loc=trawl%3A',site,'%3A1&typeData=raw')
  
  temp <- read.csv(url) %>% mutate(site = site) %>%
    rename('Catch' = 2, 'Samples' = 3)
  
  if (ncol(temp) >= 3) {
    trawlList[[index]] <- temp
  }
}

data2 <- bind_rows(trawlList) %>% 
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Date)) %>%
  mutate(WY = get_fy(Date, opt_fy_start = '10-01')) %>%
  mutate(site = case_when(site == 'SR055' ~ 'Sherwood',
                          site == 'SJ054' ~ 'Mossdale',
                          site == 'SB018' ~ 'Chipps'))

summary2 <- data2 %>% group_by(WY, site) %>%
  summarize(`Raw Catch` = sum(Catch, na.rm = TRUE),
            `Number of Hauls` = sum(Samples, na.rm = TRUE)) %>%
  mutate(`CPUE (fish/haul)` = `Raw Catch`/`Number of Hauls`) %>%
  gather(key = 'Method', value = 'Catch', 3:5) %>%
  mutate(Method = factor(Method, levels = c('CPUE (fish/haul)', 'Number of Hauls', 'Raw Catch')),
         site = factor(site, levels = c('Sherwood', 'Mossdale', 'Chipps'), 
                       labels = c('Sacramento River (Sherwood)', 'San Joaquin River (Mossdale)',
                                        'Chipps Island'))) %>%
  filter(WY > 2009)

graph2 <- ggplot(summary2) +
  geom_col(aes(x = factor(WY), y = Catch, fill = site), color = 'black') + 
  labs(x = 'Water Year', y = 'Count', fill = '') +
  facet_wrap(~Method, ncol = 1, scales = 'free_y') + 
  scale_fill_viridis(discrete=TRUE, option="cividis") +
  theme_bw() +
  theme(plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'bottom')
graph2

ggsave(graph2, file = 'Salmonids/appendix_outputs/Trawlgraph.png', units = 'in', height = 7.5, width = 6.5)

#################### Historic natural vs. hatchery Steelhead loss and FL histograms over time

#importing, cleaning, and summarizing data from SacPAS
loss2 <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Aall&dnaOnly=no&age=no') %>%
  mutate(WY = get_fy(as.Date(Sample.Time), opt_fy_start = '10-01')) %>%
  filter(Adipose.Clip %in% c('Clipped', 'Unclipped'))


#making graph for historic loss comparison
historic <- loss2 %>%
  group_by(WY, Adipose.Clip) %>%
  summarize(Loss = sum(Loss)) %>%
  ggplot() + 
  geom_col(aes(x = factor(WY), y = Loss, fill = Adipose.Clip), position = 'dodge', color = 'black') +
  labs(x = 'Water Year', y = 'Total Loss') +
  scale_fill_viridis(discrete=TRUE, option="cividis") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_blank(),
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        legend.position = c(0.9, 0.8))
historic

ggsave(historic,
       filename = file.path(here::here("Salmonids/appendix_outputs/historic_SH_loss_barplot.png")), 
       width = 8, height = 6, units = "in")

#making graph for hatchery vs wild FL comparison with a few options

size2 <- loss2 %>% #facets by clipped vs unclipped with WY on Y axis
  filter(Length < 750) %>%
  filter(WY > 2004) %>%
  ggplot(aes(x = Length, y = factor(WY), fill = Adipose.Clip)) +
  geom_density_ridges(scale = 1.5, draw_baseline = FALSE) +
  facet_wrap(~Adipose.Clip) +
  labs(x = 'Fork Length (mm)', y = 'Water Year') +
  theme_bw() +
  theme(legend.position = 'none',
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)))
size2

ggsave(size2,filename = file.path(here::here("Salmonids/appendix_outputs/size_distribution_historical.png")), 
       width = 6, height = 7, units = "in")

size3 <- loss2 %>% #facets by clipped vs unclipped with WY2024 overlaying historic WYs combined
  filter(Length < 750) %>%
  ggplot() +
  geom_density(filter(loss2, WY != 2024), 
               mapping = aes(x = Length), fill = 'darkgrey', color = NA) +
  geom_histogram(filter(loss2, WY == 2024), 
                 mapping = aes(x = Length, y=after_stat(density)), 
                 fill = 'steelblue3',
                 color = '#333333',
                 bins = 50,
                 alpha = 0.6) +
  labs(x = 'Fork Length (mm)', y = 'Proportional Density') +
  facet_wrap(~Adipose.Clip, ncol = 1) +
  theme_bw() +
  theme(legend.position = 'none',
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)))
size3

ggsave(size3,filename = file.path(here::here("Salmonids/appendix_outputs/size_distribution.png")), 
       width = 6, height = 7, units = "in")
