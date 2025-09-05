library(tidyverse)
library(lubridate)
library(DiagrammeR)
library(htmlwidgets)
library(ggplot2)
library(readr)
library(plotly)
library(readxl)
library(vistime)
library("scales")
library("cowplot")


library(readxl)
actions <-  read_csv("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2022_outlookdata6282022format.csv")
View(actions)

actions <- actions %>% select(event,group,start,end,color,category,notes) 


actions$start <- as.Date(actions$start)
actions$end <- as.Date(actions$end)


library(plotly)
d1 <- vistime(actions, optimize_y = T, col.group = "group", col.color = "color", linewidth = 25)
d1 %>% layout(title = "Disruptions of Sampling",
              xaxis = list(title = "Date", fixedrange=F, type = 'date', dtick ="M1",tickformat = "%b"))
plot(d1)

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


###################################################################################################################
library(readxl)
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
