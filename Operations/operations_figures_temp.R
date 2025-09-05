#Code by Nick Bertrand
#nbertrand@usbr.gov
#updated by Chase Ehlo
#cehlo@usbr.gov

#this script will create the graph for the OMR index, exports, and DCC gates figure


library(readxl)
library(tidyverse)
library(scales)
library(ggpubr)
library(CDECRetrieve)
library(patchwork)

#data provided by Reclamation CVO and DWR
wy <- year(Sys.Date()) #will need to switch to this code once spreadsheet is updated and delivered

###################################################
#automating the reading in of relevant data files
library(readxl)

data_import <- read_excel('ControllingFactors/CVP Delta OPS.xlsx', skip = 1) %>%
  select(date = 1, status = 2, JPP = 4, CCF = 6, DCC = 5,
         omr_usgs_1 = 7, omr_usgs_5 = 8, omr_usgs_14 = 9,
         omr_1 = 10, omr_5 = 11, omr_7 = 12, omr_14 = 13) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(date),
         date <= as.Date('2025-06-30')) %>%
  mutate(across(6:12, as.numeric))

#############################
#graphing exports and OMRI
#############################

#plots OMR data
omr_plot <- data_import %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = omr_1)) +
  labs(y = 'Daily OMR Index (cfs)') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  #geom_col(aes(y = exports, fill = Facility))
omr_plot

#plots export data
export_plot <- data_import %>%
  pivot_longer(names_to = 'Facility',
               values_to = 'exports',
               3:4) %>%
  mutate(Facility = factor(Facility, levels = c('CCF', 'JPP'),
                           labels = c('Clifton Courth Inflow',
                                      'Jones Pumping Plant'))) %>%
  ggplot(aes(x = date)) +
  labs(y = 'Exports (cfs)') +
  geom_col(aes(y = exports, fill = Facility)) +
  scale_fill_manual(values = c('#0072B2', '#E69F00')) +
  scale_x_date(date_breaks = '4 weeks', date_labels = '%b') +
  theme_bw() +
  theme(legend.position = 'bottom')
export_plot

final_figure <- omr_plot/export_plot + plot_layout(height = c(1.5,1))
final_figure

ggsave(final_figure, file = 'Operations/outputs/omr_exports.png', height = 5, width = 8)

####################################
#graphing DCC gate and catch indices
####################################

#pulling data from SacPAS
trawlurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year='
                   ,wy-1,'&species=CHN%3AWinter&loc=trawl%3ASR055%3A1&typeData=index')
seineurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year='
                   ,wy-1,'&species=CHN%3AWinter&loc=seine%3Asacbeach%3A1&typeData=index')
klurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year='
                    ,wy-1,'&species=CHN%3AWinter&loc=trap%3AKNL%3A0&typeData=index')
  

seine_import <- read_csv(seineurl) %>%
  select(Date, index = 4) %>%
  mutate(sample = 'Seines',
         Date = as.Date(Date)) %>%
  filter(!is.na(Date))

trawl_import <- read_csv(trawlurl) %>%
  select(Date, index = 4) %>%
  mutate(sample = 'Trawls',
         Date = as.Date(Date)) %>%
  filter(!is.na(Date))

kl_import <- read_csv(klurl) %>%
  select(Date, index = 2) %>%
  mutate(sample = 'KL RST',
         Date = as.Date(Date)) %>%
  filter(!is.na(Date))

all_index <- bind_rows(seine_import, trawl_import, kl_import) %>%
  mutate(sample = factor(sample, levels = c('KL RST', 'Seines', 'Trawls'),
                         labels = c('Knights Landing RST', 'Sacramento Seines', 'Sacramento Trawls')))

dcc_graph <- data_import %>%
  ggplot() +
  geom_rect(aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf, fill = DCC)) +
  scale_fill_manual(
    values = c('O' = 'darkgrey'),  # Only include 'C'
    labels = c('O' = 'Opened'),    # Only label 'C'
    na.value = "transparent",
    drop = TRUE                   # Drop unused levels
  ) +
  geom_point(all_index, mapping = aes(x = Date, y = index, color = sample), size = 1) +
  labs(y = 'Catch Index', fill = 'DCC Gate Status') +
  scale_x_date(
    date_breaks = '4 weeks', 
    date_labels = '%b', 
    limits = c(as.Date('2024-10-01'), as.Date('2025-06-30'))
  ) +
  scale_color_manual(values = c('#0072B2', '#E69F00', '#009E73')) +
  facet_wrap(~sample, ncol = 1) +
  guides(color = 'none') +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.background = element_rect(fill = NA),
        strip.text = element_text(face = 'bold'))
dcc_graph
ggsave(dcc_graph, file = 'Operations/outputs/dcc_gates.png', height = 4, width = 6)

####################################
#graphing different OMR indices
####################################

omr_all <- data_import %>%
  pivot_longer(names_to = 'index', values_to = 'flow', c(9,10,12)) %>%
  mutate(index = factor(index, levels = c('omr_1', 'omr_5', 'omr_14'),
                         labels = c('OMR Index 1-day',
                                    'OMR Index 5-day Mean',
                                    'OMR Index 14-day Mean')))

omr_plot <- ggplot(omr_all, aes(x = date, y = flow, color = index, linetype = index))+
  geom_line(linewidth = 1) +
  scale_color_manual(values = c('#0072B2', '#E69F00', '#009E73')) +
  labs(y = 'Flows (cfs)', x = 'Date', color = 'Indexes', linetype = 'Indexes') +
  theme_bw() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme(legend.position = 'bottom')
omr_plot
ggsave(omr_plot, file = 'Operations/outputs/omr_indexes.png', height = 5, width = 8)
