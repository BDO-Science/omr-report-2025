#WordFileDataExtraction.R----

# Nick Bertrand
# Start Date: Mon Jun 24 10:24:59 2024

#About----
#Project: 2024 OMR seasonal Report

#Purpose:#this script will extract the survey interruptions table from the weekly outlooks from the whole season 

#this script also includes the generation of the Sampling disruptions figure.
#this is the only script needed to generate the figure. 

#Libraries ----
library(tidyverse)
library(readr)
library(docxtractr)
library(dplyr)
library(patchwork)

#Export Multiple Files ----
#A function to iterate through multiple files in a directory
#creates the dataframe to dump the data in
#this code section has been glitchy in R studio, it may need to be run, 
#twice sequentially to generate the data frame.
survey <- data.frame()
#reached to the directory with all the Outlook .docx files and creates a list of file names 
filenames <- Sys.glob(file.path("SurveyInterruptions/Data/WY2025_Outlooks/*.docx")) #C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2023Outlooks/*.docx")
#filenames <- Sys.glob("C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/omr_report_2024/SurveyInterruptions/Data/WY2024_Outlooks/*.docx")
#view(filenames)

#test <-c("C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/omr_report_2024/SurveyInterruptions/Data/WY2024_Outlooks/20231003 fish and water operations outlook.docx")
#function uses file name list
for ( x in filenames) {
  # to read in the data
  doc <- read_docx(x)
  #data is extracted based on table number
  #assigns number to all tables by counting them
  last_num <- docx_tbl_count(doc)
  #uses the number of the last table in the outlook
  #number assigned is extracted .
  data <- docx_extract_tbl(doc, last_num) %>% mutate(week = names(data)[3])
  #View(data)
 
  data$week <- gsub("Notes..as.of.", "", data$week)
  #'.' does not work but for some reason '//.' does as described by stack overflow
  #view(data)
  data$week <- gsub('.','/',data$week, fixed = TRUE)
  #view(data)
  #cleans up date formating
  data$week <- gsub('/2024/','/2024',data$week)
  data$week <- gsub('/2025/','/2025',data$week)
  #data$week <- as.Date(data$week, "%m/%d/%y")
  #renames Columns
  data3 <- data %>% rename(Survey = 1, Region = 2, Notes = 3, Status = 4, Week = 5)
  #binds extracted data back to the dump dataframe
  survey <- rbind(survey,data3)
}
#View(survey)

#this formating corrects for the lack of zeros in some of the dates
dates <- data.frame(unique(survey2$date))

survey2 <- survey %>%
  mutate(date = mdy(Week), 
         Status = factor(Status, levels = c(1,2,4), labels = c('Active', 'Partially Active', 'Not Active')),
         Survey = if_else(Survey == '20-mm Survey', '20mm Survey', Survey))
graph_survey <- survey2 %>%
  filter(!grepl('RST', Survey)) %>%
  ggplot(aes(x = date, y = Survey, fill = Status)) +
  geom_tile(color = 'darkgrey') +
  scale_fill_viridis_d() +
  labs(x = 'Week',
       title = 'General Surveys') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
graph_survey

graph_rst <- survey2 %>%
  filter(grepl('RST', Survey)) %>%
  ggplot(aes(x = date, y = Survey, fill = Status)) +
  geom_tile(color = 'darkgrey') +
  scale_fill_viridis_d(option = 'D') +
  labs(x = 'Week',
       title = 'Rotary Screw Traps') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.y = element_blank())
graph_rst

all_graphs <- graph_survey/graph_rst +
  plot_layout(heights = c(5,2))
  #plot_annotation(tag_levels = 'A')
all_graphs

ggsave(all_graphs, file = 'SurveyInterruptions/Viz_Output/survey_interrupt.png', height = 8, width = 9)
