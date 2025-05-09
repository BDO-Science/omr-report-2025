library(officer)
library(janitor)
library(ggtext)
library(tidyverse)


# 2.0 EXTRACT THE DOCX CONTENTS -----
doc <- read_docx("WY2022Outlooks/20211005 fish and water operations outlook .docx")

content_tbl <- docx_summary(doc) %>% as_tibble()

table_content_tbl <- content_tbl %>%
  filter(content_type == "table cell")

table_content_tbl

View(table_content_tbl)

table_content_tbl.2 <-table_content_tbl %>%  filter(doc_index == "35")
view(table_content_tbl.2)

# 3.0 FORMAT THE DATA ----

# * Table Headers ----
table_header <- table_content_tbl %>%
  filter(is_header) %>%
  pull(text)

view(table_header)

# * Table Contents ----
lecture_analysis_tbl <- table_content_tbl %>%
  filter(!is_header) %>%
  select(text, row_id, cell_id) %>%
  pivot_wider(names_from = cell_id, values_from = text) %>%
  select(-row_id) %>%
  set_names(table_header) %>%
  clean_names() 
view(lecture_analysis_tbl)
