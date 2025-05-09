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

# Set working directory ----
#set to location of root object to highest tier directory
getwd()
root <- "C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/omr_report_2024/SurveyInterruptions"
setwd(root)
getwd()
#these root object use directories 
data_root<-file.path(root,"Data")
code_root <- file.path(root,"R_scripts")
table_output_root <- file.path(root,"Table_Output")
viz_output_root <- file.path(root,"Viz_Output")

#Export Multiple Files ----
#A function to iterate through multiple files in a directory
#creates the dataframe to dump the data in
#this code section has been glitchy in R studio, it may need to be run, 
#twice sequentially to generate the data frame.
survey <- data.frame()
#reached to the directory with all the Outlook .docx files and creates a list of file names 
filenames <- Sys.glob(file.path(data_root,"WY2024_Outlooks/*.docx")) #C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2023Outlooks/*.docx")
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
  data$week <- gsub('/2023/','/2023',data$week)
  data$week <- gsub('/2024/','/2024',data$week)
  #data$week <- as.Date(data$week, "%m/%d/%y")
  #renames Columns
  data3 <- data %>% rename(Survey = 1, Region = 2, Notes = 3, Status = 4, Week = 5)
  #binds extracted data back to the dump dataframe
  survey <- rbind(survey,data3)
}
#View(survey)

#this formating corrects for the lack of zeros in some of the dates
#survey$Week <- format(as.Date(survey$Week, format = "%m/%d/%Y"), "%d/%m/%Y")
survey$Week <- format(as.POSIXct(survey$Week, format = "%m/%d/%Y"), "%Y-%m-%d")
#watch the date formatting and time zone here 
survey$Week <- as.POSIXct(survey$Week, tz="GMT")

View(survey)
#class(survey$Week)
#safety check on formatting
survey$Week


#Extract One outlook File Test code ----

#extracts a table from one file, just wanted to save this code
#git hub example for read_docx is broken, code below worked
doc <- read_docx(filenames <- Sys.glob(file.path(data_root,"WY2024_Outlooks/20231003 fish and water operations outlook.docx")))

#assigns number to all tables by counting them
last_num <- docx_tbl_count(doc)
#uses the number of the last table in the outlook
#number assigned is extracted .
data <- docx_extract_tbl(doc, last_num) %>% mutate(week = names(data)[3])
#View(data)
# data2 <- data1 %>% mutate(week = names(data1)[3])
# View(data2)

data$week <- gsub("Notes..as.of.", "", data$week)
#'.' does not work but for some reason '//.' does as described by stack overflow
#view(data)
data$week <- gsub('.','/',data$week, fixed = TRUE)
#view(data)
data$week <- as.Date(data$week, "%m/%d/%y")
view(data)
#renames Columns
data <- data %>%rename(Survey = 1, Region = 2, Notes = 3, Status = 4, Week = 5)

view(data)



#Gantt Chart Input ----
#the code below converts the extracted data from the outlooks into a format that can be 
#read into the gant chart script

##Seven day Start & End date file ----
#I created a file in excel with the 7 day intervals in which the outlooks are
#written.

library(readxl)
StartandEndDates <- read_excel(file.path(data_root,"WY2024StartandEndDates.xlsx"))

#View(StartandEndDates)

#safety check on formatting
survey$Week
#safety check on formatting of the dates
class(StartandEndDates$Start)
##Start and end date join ----

#data frame is joined and some minor edits are done to 
#format it for the gantt chart input
gant <- left_join(survey, StartandEndDates, join_by(Week == Start), keep = TRUE)

view(gant)

#adds empty column titled event
gant$event <- NA

gant_input <- gant %>% 
  select(event, Start, End, Survey, Notes, Status) %>% 
  rename(start=Start, end = End, group = Survey, notes = Notes, category = Status)  
#view(gant_input)

##Save Input File ----
#minor formatting edits will need to be made in excel due to oddities in reporting
#that happen during the season. 
#this is also a good datafile to create a data check in case outputs look weird later

#commented out to avoid overwriting
#write.csv(gant_input, file =file.path(data_root, "WY2024dataExtraction.csv"))


#Re-import file ----
#This imports the renamed, formatted, and edited file 
library(readr)
#make sure the date is formatted in 2024-01-01 in the excel file before import
WY2024dataFormatted <- read_csv(file.path(data_root,"WY2024dataFormatted.csv"))
#View(WY2023dataFormatted)


#check class of columns if errors are produced below
#class(WY2023dataFormatted$category)

WY2024dataFormatted$category <- as.numeric(WY2024dataFormatted$category)
##Import Color Code dataframe ----
#data frame with color codes to assign to status values (renamed category)
library(readr)
ColorCodes <- read_csv(file.path(data_root,"ColorCodes.csv"))
#View(ColorCodes)
class(ColorCodes$category)
##Join color codes----
#joins the color codes to the formatted datafile
inter_dat <-left_join(WY2024dataFormatted, ColorCodes, by = join_by("category"))
view(inter_dat)

#Gantt Chart ----
#libraries
library(DiagrammeR)
library(htmlwidgets)
library(ggplot2)
library(readr)
library(plotly)
library(readxl)
library(vistime)
library("scales")
library("cowplot")

#change object name for this code chunk
actions <- inter_dat

actions$start <- as.Date(actions$start)
actions$end <- as.Date(actions$end)

##Full Gantt Chart----
library(plotly)
d1 <- vistime(actions, optimize_y = T, col.group = "group", col.color = "color", linewidth = 25)
d1 %>% layout(title = "Disruptions of Sampling",
              xaxis = list(title = "Date", fixedrange=F, type = 'date', dtick ="M1",tickformat = "%b"))

#review the first run of this plot for any weeks missing as 
#a result of the dates in the outlook table not being correct.
plot(d1)
d1
#use the nap shot feature in the full expanded viewer in rstudio to get a .png file

##RST Gantt Chart ----

rst_list <- c("Red Bluff Diversion Dam Rotary Screw Trap (RST)", 
              "Knights Landing RST",
              "Tisdale RST",
              "GCID RST",
              "Lower Sacramento RST",
              "Feather River (upper DWR) RST",
              "Feather River (lower CDFW) RST",
              "Caswell RST",
              "Butte Creek RST/Diversion Trap",
              "Lower American River at Watt Ave RST",
              "Yuba River (Hallwood) RST")

rst <- actions %>% filter(group %in% rst_list)

g_rst <- vistime(rst, optimize_y = T, col.group = "group", col.color = "color", linewidth = 25)
g_rst %>% layout(title = "Disruptions of Sampling",
              xaxis = list(title = "Date", fixedrange=F, type = 'date', dtick ="M1",tickformat = "%b"))


##Survey Gantt Chart----

surv <- actions %>% filter(!group %in% rst_list)

g_surv <- vistime(surv, optimize_y = T, col.group = "group", col.color = "color", linewidth = 25)
g_surv %>% layout(title = "Disruptions of Sampling",
                 xaxis = list(title = "Date", fixedrange=F, type = 'date', dtick ="M1",tickformat = "%b"))



#this is the end of the code that must be used.

#Legend----
#The code below can create a new legend image if needed.
#I have just been resusing the legend already in the report but if not the code below can regnerate.
### Create a legend image 
data_legend <- actions %>%
  distinct(category, .keep_all=T) %>%
  arrange(category) %>% 
  select(group, start, end, color, category, notes) %>% 
  rename("event"= "category") %>% 
  mutate(event = ifelse(event == "0", "No Disruption",
                        ifelse(event == "1", "No Sampling",
                               ifelse(event == "2", "Partial Disruption",
                                      ifelse(event == 3, "Inactive", "no")))))
data_legend

data_legend$start <- as.Date("2020-01-01")
data_legend$end <- as.Date("2020-01-02")
data_legend$Patient <- "Key"
data_legend
plot_legend <- gg_vistime(data = data_legend,
                          col.group = "Patient",
                          col.event = "event",
                          show_labels = TRUE,
                          linewidth = 20,
                          title = "Legend")
plot_legend

# Tweak the legend plot
plot_legend <- plot_legend + theme_void() +
  ggplot2::theme(
    plot.title = element_text(size=11),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
plot_legend


### Combine the main plot and legend into a single figure
plot_combined <- plot_grid(plot, plot_legend,
                           rel_widths = c(1, 0.15))
plot_combined

#actions <- read_excel("WY2021/WY2021_Disruptions.xlsx", 
#                      sheet = "SaMT")
#View(WY2021_Disruptions)

#View(actions)
actions$start <- as.Date(actions$start)
actions$end <- as.Date(actions$end)


library(plotly)
d1 <- vistime(actions, optimize_y = T, col.group = "group", col.color = "color", linewidth = 25)
d1 %>% layout(title = "Disruptions of Sampling",
              xaxis = list(title = "Date", fixedrange=F, type = 'date', dtick ="M1",tickformat = "%b"))

plot(d1)
