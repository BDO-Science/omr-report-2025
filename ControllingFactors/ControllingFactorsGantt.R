library(tidyverse)
library(lubridate)
library(DiagrammeR)
library(htmlwidgets)
library(ggplot2)
library(readr)
library(vistime)
library(readxl)
library(plotly)

# #ControlFactorTime <- read_delim("ControlFactorTime.txt", 
#                                 "/t", escape_double = FALSE, col_types = cols(start = col_character()), 
#                                 trim_ws = TRUE)


ControlFactorTime <- read_excel("ControllingFactors/ControllinginputFile_20240725.xlsx", 
                                            sheet = "Controlling Periods 2024")

#View(ControlFactorTime)

ControlFactorTime$start <- as.Date(ControlFactorTime$start)
ControlFactorTime$end <- as.Date(ControlFactorTime$end)


t1 <- vistime(ControlFactorTime, optimize_y = T, col.group = "group", col.color = "color",show_labels = FALSE)
#t2<- t1 %>% layout(title = "Time Series with Custom Date-Time Format", xaxis = list(type = 'date', tickformat = "%Y",autotick = F, dtick = "%m"))

#t2

t1 %>% layout(xaxis=list(fixedrange=TRUE, tickfont=list(size=50, color="black",type = 'date', tickformat = "%Y",autotick = F, dtick = "%m")), 
              yaxis=list(fixedrange=TRUE, tickfont=list(size=75, color="black"), tickangle=30, mirror = FALSE, range = c(0.7, 3.5), showgrid = T))
t1



##################################
############
#####
#this file was just made in excel based on the information in the Controlling Factors Table WY 2023_v4.xlsx file

BalEx <- read_excel("ControllingFactors/BalancedVSExcessWY2024.xlsx")

#View(BalEx)


b1 <- vistime(BalEx, optimize_y = T, col.group = "group", col.color = "color",show_labels = FALSE)
#t2<- t1 %>% layout(title = "Time Series with Custom Date-Time Format", xaxis = list(type = 'date', tickformat = "%Y",autotick = F, dtick = "%m"))

#t2

b1 %>% layout(xaxis=list(fixedrange=TRUE, tickfont=list(size=30, color="black",type = 'date', tickformat = "%Y",autotick = F, dtick = "%m")), 
              yaxis=list(fixedrange=TRUE, tickfont=list(size=30, color="black"), tickangle=30, mirror = FALSE, range = c(0.7, 3.5), showgrid = T))
b1
