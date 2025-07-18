library(tidyverse)
library(readxl)

data_import <- read_excel('ControllingFactors/CVP Delta OPS.xlsx', skip = 1) %>%
  select(date = 1, status = 2, JPP = 4, CCF = 6, DCC = 5,
         omr_usgs_1 = 7, omr_usgs_5 = 8, omr_usgs_14 = 9,
         omr_1 = 10, omr_5 = 11, omr_7 = 12, omr_14 = 13) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(date))

dcc_temp <- select(data_import, 1,5) %>%
  filter(DCC == 'O',
         date < as.Date('2025-07-01') &
           date > as.Date('2024-09-30')) %>%
  mutate(Factor = 'DCC Gate Open') %>%
  select(3,Date = 1)

controlling <- read_csv('ControllingFactors/controlling-factors_WY2025.csv') %>%
  mutate(Date = mdy(Date)) %>%
  bind_rows(dcc_temp) %>%
  na.omit()

delta_condition <- data_import %>%
  mutate(condition = case_when(status == 'B' ~ 'Balanced', 
                               status == 'E' ~  'Excess',
                               status == 'E/R' ~ 'Excess w/ Restrictions')) %>%
  filter(!is.na(condition),
         date <= as.Date('2025-06-30'))


condition_graph <- ggplot(delta_condition, aes(x = date, y = condition)) +
  geom_tile(fill = 'black') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %Y') +
  theme_bw() +
  labs(x = 'Date') +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
condition_graph

controlling_graph <- ggplot(controlling, aes(x = Date, y = Factor)) +
  geom_tile(fill = 'black') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %Y') +
  theme_bw() +
  labs(x = 'Date') +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
controlling_graph
ggsave(condition_graph, file = 'ControllingFactors/excess_balance_fig.png', height = 2, width = 6)
ggsave(controlling_graph, file = 'ControllingFactors/control_factor_fig.png', height = 2.5, width = 7)

