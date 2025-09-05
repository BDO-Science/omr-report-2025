library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(padr)
library(zoo)
library(purrr)
library(patchwork)
library(readr)
library(readxl)
library(gridExtra)
library(deltafish)

# This file pulls and plots the environmental data for Smelt OMR Season for the 2023 OMR Seasonal Report.
# modified by G.Easterbrook (geasterbrook@usbr.gov), data pulling code derived from code by N. Bertrand
# Last edited by L. McCormick (lmccormick@usbr.gov) for 2025 OMR report

#qwest data-- will do qwest once we receive data file 

#qwest0 <- read_excel(here::here("ControllingFactors", "Controlling Factors Table WY 2023.xlsx"), sheet = "OCOD Data 2023")
#qwest <- qwest0 %>%
#dplyr::select(Date, QWESTcfs) %>%
#mutate(Date = ymd(Date))

#qwest0 <- read_excel("C:/My Projects/SMT/OMR seasonal report/Controlling Factors Table WY 2034.xlsx", 
#sheet = "OCOD Data 2023")

qwest0 <- read_excel("C:/My Projects/SMT/OMR seasonal report/Controlling Factors Table WY 2024.xlsx", 
                     sheet = "OCOD Data 2024")

qwest <- qwest0 %>%
  select(Date, QWESTcfs) %>%
  mutate(Date = ymd(Date))
#qwest <- qwest0 %>%
#select(Date, QWESTcfs) %>%
#mutate(Date = ymd(Date))


# sets the dates to be pulled from cdec for the OMR season

start.date <- "2024-10-01"
start.date.FPT <- "2024-10-01"
end.date <- "2025-06-30"

# Series of cdec queries to pull data needed to fill out the reports datafile ------------
clc.C <- cdec_query("CLC", "146", "D", start.date, end.date)%>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

OBI.fnu <- cdec_query("OBI", "221", "D", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

HOL.fnu.hr <- cdec_query("HOL", "221", "H", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

HOL.fnu <- HOL.fnu.hr %>%
  group_by(date) %>%
  summarize(parameter_value= mean(parameter_value, na.rm=TRUE))
HOL.fnu <- HOL.fnu[-c(274),] # remove weird last line

# OBI.fnu.event <- cdec_query("OBI", "221", "E", start.date, end.date) %>%
#   rename(date = datetime) %>%
#   mutate(date = as.Date(date))

OSJ.fnu <- cdec_query("OSJ", "221", "D", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

FPT.cfs <- cdec_query("FPT", "20", "D", start.date.FPT, end.date)%>%
  rename(date = datetime)%>%
  mutate(date = as.Date(date))

FPT.fnu <- cdec_query("FPT", "221", "D", start.date.FPT, end.date)%>%
  rename(date = datetime)%>%
  mutate(date = as.Date(date))

MSD.c <- cdec_query("MSD", 146, "D", start.date, end.date) %>% 
  mutate(date = date(datetime))

PPT.c <- cdec_query("PPT", 146, "D", start.date, end.date)%>% 
  mutate(date = date(datetime)) 


#### Secchi depth data (SLS and 20mm surveys)
### If deltafish data are updated, uncomment below
# create_fish_db(update = TRUE) # takes a minute to run- large db
# con <- open_database()
# surv <- open_survey(con)
# surv_20_sls <- surv %>%
#   filter(Source %in% c("20mm", "SLS")) %>%
#   select(Source, Station, Latitude, Longitude, Date, Datetime, Survey, Tide, TurbidityNTU, TurbidityFNU, Secchi, Secchi_estimated) %>%
#   collect_data() # ONLY USE if you are ONLY running the survey data. If you combine with fish data, don't run
# 
# # correcting units in secchi (20mm is in cm, SLS is in m)
# surv_20_sls$CorSec <- surv_20_sls$Secchi
# surv_20_sls$CorSec[which(surv_20_sls$Source== "SLS")] <-surv_20_sls$Secchi[which(surv_20_sls$Source== "SLS")]*100
# 
# # filter to only south delta stations
# sd_surv <- surv_20_sls %>%
#   filter(Station %in% c(809, 812, 901, 815,919, 902, 906, 915, 914, 910, 918, 912))
# 
# # calculate secchi depth avg
# mns <- sd_surv %>%
#   group_by(Date, Source) %>%
#   summarize(m_secchi= mean(CorSec, na.rm=TRUE))
# 
# close_database(con)

# Manually add secchi depth data
sd <- read.table("SecchiDepth_2025.txt", header = TRUE, sep = "\t")
sd$Date <- as.Date(sd$Date)


#### Old method  of creating data

# DateSeriesWY2023 <- data.frame(date = seq(as.Date(start.date),as.Date(end.date), by = "1 days"))
# date.key = DateSeriesWY2023

#### Clean up data and make sure not too many dates missing -------------------------------

OBI.fnu.smelt <- OBI.fnu %>%
  select(date, parameter_value) %>% rename(OBI.fnu.smelt = parameter_value) %>%
  pad #double check all dates in there

(OBI.fnu.smelt %>% filter(is.na(OBI.fnu.smelt))) # 8 days missing

HOL.fnu.smelt <- HOL.fnu %>%
  select(date, parameter_value) %>% rename(HOL.fnu.smelt = parameter_value) %>%
  pad #double check all dates in there

(HOL.fnu.smelt %>% filter(is.na(HOL.fnu.smelt))) # 3 days missing

OSJ.fnu.smelt <- OSJ.fnu %>%
  select(date, parameter_value) %>% rename(OSJ.fnu.smelt = parameter_value) %>%
  pad #double check all dates in there

(OSJ.fnu.smelt %>% filter(is.na(OSJ.fnu.smelt))) # 6 days missing

FPT.cfs.smelt <- FPT.cfs %>% 
  select(date, parameter_value) %>% rename(FPT.cfs.smelt = parameter_value) %>%
  pad %>%
  arrange(date) %>%
  mutate(FPT.cfs.smelt = as.numeric(FPT.cfs.smelt),
         FPT.3day.cfs = rollapplyr(FPT.cfs.smelt,3,  mean, align = "right", partial =T)) %>%
  filter(date >= start.date)

(FPT.cfs.smelt %>% filter(is.na(FPT.cfs.smelt))) # 11 days missing in 2025

FPT.fnu.smelt <- FPT.fnu %>%
  select(date, parameter_value) %>% rename(FPT.fnu.smelt = parameter_value) %>%
  pad %>%
  arrange(date)%>%
  mutate(FPT.fnu.smelt = as.numeric(FPT.fnu.smelt),
         FPT.3day.fnu = rollapplyr(FPT.fnu.smelt,3, mean, align = "right", partial =TRUE)) %>%
  filter(date >= start.date)

(FPT.fnu.smelt %>% filter(is.na(FPT.fnu.smelt))) # 5 days missing in 2025

CLC.C.smelt <- clc.C %>% 
  select(date, parameter_value) %>% rename(CLC.C.smelt = parameter_value) %>%
  #mutate(CLC.F.smelt = (CLC.C.smelt * 9/5) + 32) %>%
  pad

(CLC.C.smelt %>% filter(is.na(CLC.C.smelt))) # 8 days missing

MSD.C.salmon <- MSD.c %>%
  group_by(date) %>% 
  mutate(msd.c = mean(parameter_value,na.rm =TRUE)) %>% 
  ungroup() %>%
  select(date, msd.c) %>% 
  distinct() %>% 
  drop_na() %>%
  pad() %>%
  arrange(date) 

PPT.C.salmon <- PPT.c %>%
  group_by(date) %>% 
  mutate(ppt.c = mean(parameter_value,na.rm =TRUE)) %>% 
  ungroup() %>%
  select(date, ppt.c) %>% 
  distinct() %>% 
  drop_na() %>%
  pad() %>%
  arrange(date) 


#Farenheit
# MSD.F.salmon <- MSD.f %>%
#   group_by(date) %>% 
#   mutate(msd.F = mean(parameter_value,na.rm =TRUE)) %>% 
#   ungroup() %>%
#   select(date, msd.F) %>% 
#   distinct() %>% 
#   drop_na() %>%
#   pad() %>%
#   arrange(date) 
# 
# PPT.F.salmon <- PPT.f %>%
#   group_by(date) %>% 
#   mutate(ppt.F = mean(parameter_value,na.rm =TRUE)) %>% 
#   ungroup() %>%
#   select(date, ppt.F) %>% 
#   distinct() %>% 
#   drop_na() %>%
#   pad() %>%
#   arrange(date) 



# Combine into one df and write ----------------------------------
smelt_env_params <- reduce(list(OBI.fnu.smelt, HOL.fnu.smelt, OSJ.fnu.smelt, FPT.cfs.smelt, FPT.fnu.smelt, CLC.C.smelt), dplyr::left_join, by = "date")
# write_csv(smelt_env_params, "EnvironmentalConditions/output/Data_smelt_environmental.csv")
smelt_env_params$date <- as.Date(smelt_env_params$date)

offramp_env_params <- reduce(list(CLC.C.smelt, MSD.C.salmon, PPT.C.salmon), dplyr::left_join, by = "date") %>% 
  filter(date >= as.Date("2025-06-01"))
# write_csv(offramp_env_params, "EnvironmentalConditions/output/Offramp_temperatures_smelt_salmon.csv")

# Make plots -----------------------------------------

theme_plots <- theme(axis.title.x = element_blank(),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 12))
#uncomment once qwest data is received 
# (plot_qwest <- ggplot(qwest) + 
#     geom_hline(yintercept = 0,  linewidth = 1, linetype = "dashed", color = "gray70") +
#     geom_line(aes(Date, QWESTcfs)) +
#     scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
#     labs(y = "QWEST (cfs)") +
#     theme_bw() +
#     theme_plots)


(plot_obi <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 12,  linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, smelt_env_params$OBI.fnu.smelt), size=0.6) +
    annotate(geom= "rect", xmin = as.Date("2025-01-15"), xmax = as.Date("2025-01-17"), 
             ymin= Inf, ymax= -Inf, color= "orange", fill= "orange", alpha=0.3)+
    geom_vline(xintercept = as.Date("2025-01-12"), color= "red", alpha=0.8)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    labs(y = "OBI Turbidity (FNU)") +
    ggtitle("A")+
    theme_bw() +
    theme_plots)

(plot_hol <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 12,  linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, smelt_env_params$HOL.fnu.smelt), size= 0.6) +
    annotate(geom= "rect", xmin = as.Date("2025-01-15"), xmax = as.Date("2025-01-17"), 
             ymin= Inf, ymax= -Inf, color= "orange", fill= "orange", alpha=0.3)+
    geom_vline(xintercept = as.Date("2025-01-12"), color= "red", alpha=0.8)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    labs(y = "HOL Turbidity (FNU)") +
    ggtitle("B")+
    theme_bw() +
    theme_plots)

(plot_osj <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 12,  linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, smelt_env_params$OSJ.fnu.smelt), size= 0.6) +
    annotate(geom= "rect", xmin = as.Date("2025-01-15"), xmax = as.Date("2025-01-17"), 
             ymin= Inf, ymax= -Inf, color= "orange", fill= "orange", alpha=0.3)+
    geom_vline(xintercept = as.Date("2025-01-12"), color= "red", alpha=0.8)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    labs(y = "OSJ Turbidity (FNU)") +
    ggtitle("C")+
    theme_bw() +
    theme_plots)

turb_bridge <- grid.arrange(plot_obi, plot_hol, plot_osj, ncol=1)



(plot_fpt1 <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 25000, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, FPT.cfs.smelt), size= 0.6) +
    annotate(geom= "rect", xmin = as.Date("2024-12-19"), xmax = as.Date("2025-01-01"), 
             ymin= Inf, ymax= -Inf, color= "orange", fill="orange", alpha=0.3)+
    geom_vline(xintercept = as.Date("2024-12-16"), color= "red", alpha=0.8)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    # geom_vline(xintercept = as.numeric(as.Date("2024-01-23")), 
    #            color = "red") +
    # geom_vline(xintercept = as.numeric(as.Date("2024-02-05")), 
    #            color = "red") +
    labs(y = "FPT Flow (cfs)", title = "A") +
    theme_bw() +
    theme_plots)

(plot_fpt2 <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 50, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, FPT.fnu.smelt), linewidth= 0.6) +
    annotate(geom= "rect", xmin = as.Date("2024-12-19"), xmax = as.Date("2025-01-01"), 
             ymin= Inf, ymax= -Inf, color= "orange", fill="orange", alpha=0.3)+
    geom_vline(xintercept = as.Date("2024-12-16"), color= "red", alpha=0.8)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    # geom_vline(xintercept = as.numeric(as.Date("2024-01-23")), 
    #            color = "red") +
    # geom_vline(xintercept = as.numeric(as.Date("2024-02-05")), 
    #            color = "red") +
    labs(y = "FPT Turbidity (FNU)", title = "B") +
    theme_bw() +
    theme_plots)

gA <- ggplotGrob(plot_fpt1)
gB <- ggplotGrob(plot_fpt2)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB)) # use this method so Y axis lines up


(plot_clc <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 25, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, CLC.C.smelt)) +
    geom_vline(xintercept = as.Date(c("2025-06-28", "2025-06-29")), 
               color= "red", linewidth= 2, alpha=0.3)+
    labs(y = "CLC Temp. (°C)", title = "C") +
    theme_bw() +
    theme_plots)

(plot_msd <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 22.2, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, offramp_env_params$msd.c)) +
    geom_vline(xintercept = as.Date(c("2025-06-18", "2025-06-25", "2025-06-26", 
                                      "2025-06-28", "2025-06-29", "2025-06-30")), 
               color= "red", linewidth= 2, alpha=0.3)+
    labs(y = "MSD Temp. (°C)", title = "A") +
    theme_bw() +
    theme_plots)

(plot_ppt <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 22.2, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, ppt.c)) +
    geom_vline(xintercept = as.Date(c("2025-06-01", "2025-06-02", "2025-06-03",
                                      "2025-06-04", "2025-06-05", "2025-06-14",
                                      "2025-06-15")), 
               color= "red", linewidth= 2, alpha=0.3)+
    labs(y = "PPT Temp. (°C)", title = "B") +
    theme_bw() +
    theme_plots)


gA <- ggplotGrob(plot_msd)
gB <- ggplotGrob(plot_ppt)
gC <- ggplotGrob(plot_clc)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB,gC)) # use this method so Y axis lines up
#(plot_offramp <- plot_msd/plot_ppt/plot_clc)


ggplot(sd, aes(x=Date, y=AvgSecchi.cm))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 100, linewidth = 1, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = as.Date("2025-02-25"), 
             color= "red", linewidth= 1, alpha=0.3)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("South Delta average Secchi depth (cm)")+
  theme_bw() +
  theme_plots


# Write plots------------------------------------------
# tiff("EnvironmentalConditions/output/Figure_qwest.tiff", width = 8, height = 5, units = "in", res = 300, compression = "lzw")
# plot_qwest
# dev.off()

tiff("C:/My Projects/SMT/OMR seasonal report/Figure_obi_turbidity.tiff", width = 8, height = 5, units = "in", res = 300, compression = "lzw")
plot_obi
dev.off()

tiff("C:/My Projects/SMT/OMR seasonal report/Figure_fpt_flow_turbidity.tiff", width = 8, height = 9, units = "in", res = 300, compression = "lzw")
plot_fpt
dev.off()

# tiff("C:/My Projects/SMT/OMR seasonal report/Figure_qwest.tiff", width = 8, height = 5, units = "in", res = 300, compression = "lzw")
# plot_qwest
# dev.off()

tiff("C:/My Projects/SMT/OMR seasonal report/Figure_offramp_temperatures.tiff", width = 7, height = 9, units = "in", res = 300, compression = "lzw")
plot_offramp
dev.off()
