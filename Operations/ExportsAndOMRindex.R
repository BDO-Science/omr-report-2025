#Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will create the graph for the OMR index and exports  figure


library(readxl)
library(tidyverse)
library(scales)
library(ggpubr)
library(CDECRetrieve)

#data provided by Reclamation CVO and DWR

###################################################
#sample data for figure development
#delete once actual data is available
library(readxl)
Controlling_Factors_Table_WY_2023_v4 <- read_excel("ControllingFactors/WY2023/Controlling Factors Table WY 2023_v4.xlsx", 
                                                   col_types = c("date", "text", "numeric", 
                                                                 "numeric", "text", "text", "text", 
                                                                 "numeric", "numeric", "text", "text", 
                                                                 "text", "numeric", "numeric", "text", 
                                                                 "numeric"))
#View(Controlling_Factors_Table_WY_2023_v4)

control <- Controlling_Factors_Table_WY_2023_v4 %>% 
  select(Date,`Jones PP (cfs)`,`Clifton Court Inflow (cfs)`) 
#view(control)

OMR1day <- cdec_query("OMR", "41", "D", "2022-10-01", "2023-06-30") %>% 
  
  select(datetime, parameter_value) %>% 
  rename(Date=datetime,`OMR Index 1-day` = parameter_value)
OMR1day$Date <- as.Date(OMR1day$Date)
#View(OMR1day)

exportjoin <- left_join(control, OMR1day, by = "Date") 
#view(exportjoin)

test2 <- exportjoin
#view(test2)
exp_data <- test2 %>% gather(`Jones PP (cfs)`,
                             `Clifton Court Inflow (cfs)`,
                             `OMR Index 1-day`,
                             key = "Exports", value = "cfs")

#######################################################
#Filters data to just the exports
exp_data_filter1 <- exp_data %>% filter(Exports == "Jones PP (cfs)" | Exports == "Clifton Court Inflow (cfs)" )
#view(exp_data_filter1)
exp_data_filter2 <- exp_data %>% filter(Exports == "OMR Index 1-day")
#plots data
exp_plot <- ggplot(exp_data_filter1, aes(x = as.Date(Date), y = cfs, fill = Exports))+
  geom_bar(stat="identity")+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Flow (cfs)", breaks = pretty_breaks(n = 10))+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45), legend.position="bottom")

exp_plot

OMRIndex_plot <- ggplot(exp_data_filter2, aes(x = as.Date(Date), y = cfs))+
  geom_line()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_reverse(name = "Daily OMR Index (cfs)", breaks = pretty_breaks(n = 10))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

OMRIndex_plot


figure <- ggarrange(OMRIndex_plot,exp_plot,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure

