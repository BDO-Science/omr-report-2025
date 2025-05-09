#Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will create the graph for the OMR index figure


library(readxl)
library(tidyverse)
library(scales)
library(CDECRetrieve)
library(viridis)

#data provided by Reclamation CVO and DWR

###################################################
#sample data for figure development
#delete once actual data is available
#find and replace any "No Data" values in the excel file before importing.
# library(readxl)
# Controlling_Factors_Table_WY_2023_v4 <- read_excel("ControllingFactors/WY2023/Controlling Factors Table WY 2023_v4.xlsx", 
#                                                    col_types = c("date", "text", "numeric", 
#                                                                  "numeric", "text", "text", "text", 
#                                                                  "numeric", "numeric", "text", "text", 
#                                                                  "text", "numeric", "numeric", "text", 
#                                                                  "numeric"))
# #View(Controlling_Factors_Table_WY_2023_v4)
library(readxl)
Controlling_Factors_Table_WY_2024_v5 <- read_excel("Operations/Controlling Factors Table WY 2024-v5.xlsx")
#View(Controlling_Factors_Table_WY_2024_v5)

Controlling_Factors_Table_WY_2024_v5 %>% select(Date, `USGS Tidally Filtered Mean 5-Day OMR (cfs)`, `USGS Tidally Filtered Mean 14-Day OMR (cfs)`,`Mean 5-Day OMR Index Calculation (cfs)`, `Mean 14-Day OMR Index Calculation (cfs)`)

OMR1day <- cdec_query("OMR", "41", "D", "2023-10-01", "2024-06-30") %>% 
  select(datetime, parameter_value) %>% 
  rename(Date=datetime,`OMR Index 1-day` = parameter_value)
OMR1day$Date <- as.Date(OMR1day$Date)
#View(OMR1day)

OMRjoin <- left_join(Controlling_Factors_Table_WY_2024_v5, OMR1day, by = "Date") 
#view(OMRjoin)

test <- OMRjoin %>% select(Date,`OMR Index 1-day`,
                           `USGS Tidally Filtered Mean 5-Day OMR (cfs)`, 
                           `USGS Tidally Filtered Mean 14-Day OMR (cfs)`,
                           `Mean 5-Day OMR Index Calculation (cfs)`, 
                           `Mean 14-Day OMR Index Calculation (cfs)`) %>% 
  rename( `Tid.Filt. OMR Index 5-day` = `USGS Tidally Filtered Mean 5-Day OMR (cfs)`, 
          `Tid.Filt. OMR Index 14-day` = `USGS Tidally Filtered Mean 14-Day OMR (cfs)`,
          `OMR Index 5-day Mean` = `Mean 5-Day OMR Index Calculation (cfs)`, 
          `OMR Index 14-day Mean` = `Mean 14-Day OMR Index Calculation (cfs)`)
#view(test)

OMR_data <- test %>% 
  gather(`OMR Index 1-day`,
         `Tid.Filt. OMR Index 5-day`,  
         `Tid.Filt. OMR Index 14-day`,
         `OMR Index 5-day Mean`, 
         `OMR Index 14-day Mean`,
         key = "Indexes", value = "cfs") 


#view(OMR_data)

OMR_data$cfs <- as.numeric(OMR_data$cfs)


#######################################################
#plots all data

OMR_plot <- ggplot(OMR_data, aes(x = as.Date(Date), y = cfs, linetype = Indexes))+
  geom_line()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Flow (cfs)", breaks = pretty_breaks(n = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45), legend.position="right")

OMR_plot

#################################
#these two plots filter down the data to only a few of the indexes.

OMRlist_USGS <- c("OMR Index 1-day",
             "Tid.Filt. OMR Index 5-day",  
             "Tid.Filt. OMR Index 14-day")

OMR_data_USGS <-OMR_data %>% filter(Indexes %in% OMRlist_USGS )
#view(OMR_data_USGS)

OMR_plot_USGS <- ggplot(OMR_data_USGS, aes(x = as.Date(Date), y = cfs, linetype = Indexes))+
  geom_line()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Flow (cfs)", breaks = pretty_breaks(n = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45), legend.position="right")

OMR_plot_USGS


#plot used in the report.
OMRlist_noUSGS <- c("OMR Index 1-day",
             "OMR Index 5-day Mean", 
             "OMR Index 14-day Mean")

OMR_data_noUSGS <-OMR_data %>% filter(Indexes %in% OMRlist_noUSGS )
view(OMR_data_noUSGS)

OMR_plot_noUSGS <- ggplot(OMR_data_noUSGS, aes(x = as.Date(Date), y = cfs, color = Indexes, linetype = Indexes))+
  geom_line(linewidth = 0.7)+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Flow (cfs)", breaks = pretty_breaks(n = 10))+
  scale_linetype_manual(values=c("solid", "dotted","dashed"))+
  scale_color_manual(values = viridis(7, option = "viridis")[c(3, 1, 5)]) + 
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust = 0.5, vjust = 0.5), legend.position="bottom")

OMR_plot_noUSGS

png("Operations/ExportsAndOMRindex.png", units = "in", width = 6.5, height = 4, res = 300)
OMR_plot_noUSGS
dev.off()
