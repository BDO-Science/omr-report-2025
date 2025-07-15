library(tidyverse)
library(readxl)

data_import <- read_excel('ControllingFactors/CVP Delta OPS.xlsx', skip = 1) %>%
  select(date = 1, status = 2, JPP = 4, CCF = 6, DCC = 5,
         omr_usgs_1 = 7, omr_usgs_5 = 8, omr_usgs_14 = 9,
         omr_1 = 10, omr_5 = 11, omr_7 = 12, omr_14 = 13) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(date))

delta_condition <- data_import %>%
  mutate(condition = case_when(status == 'B' ~ 'Balanced', 
                               status == 'E' ~  'Excess',
                               status == 'E/R' ~ 'Excess w/ Restrictions')) %>%
  filter(!is.na(condition),
         date <= as.Date('2025-06-30'))


condition_graph <- ggplot(delta_condition, aes(x = date, y = condition)) +
  geom_tile(fill = 'black') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_bw() +
  labs(x = 'Date') +
  theme(axis.title.y = element_blank())
condition_graph

ggsave(condition_graph, file = 'ControllingFactors/excess_balance_fig.png', height = 2, width = 6)
