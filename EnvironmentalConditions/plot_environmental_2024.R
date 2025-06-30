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

# This file pulls and plots the environmental data for Smelt OMR Season for the 2023 OMR Seasonal Report.
# Last edited by G.Easterbrook (geasterbrook@usbr.gov), data pulling code derived from code by N. Bertrand

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

# start.date <- "2023-10-01"
# start.date.FPT <- "2023-09-29"
# end.date <- "2024-07-01"

start.date <- "2023-10-01"
start.date.FPT <- "2023-10-01"
end.date <- "2024-06-30"

# Series of cdec queries to pull data needed to fill out the reports datafile ------------
clc.C <- cdec_query("CLC", "146", "D", start.date, end.date)%>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

OBI.fnu <- cdec_query("OBI", "221", "D", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

OBI.fnu.event <- cdec_query("OBI", "221", "E", start.date, end.date) %>%
  rename(date = datetime) %>%
  mutate(date = as.Date(date))

FPT.cfs <- cdec_query("FPT", "20", "D", start.date.FPT, end.date)%>%
  rename(date = datetime)%>%
  mutate(date = as.Date(date))

FPT.fnu <- cdec_query("FPT", "221", "D", start.date.FPT, end.date)%>%
  rename(date = datetime)%>%
  mutate(date = as.Date(date))

MSD.f <- cdec_query("MSD", 25, "E", start.date, end.date) %>% 
  mutate(date = date(datetime))

PPT.f <- cdec_query("PPT", 25, "E", start.date, end.date)%>% 
  mutate(date = date(datetime)) 

SJJ.f <- cdec_query("SJJ", 25, "E", start.date, end.date)%>%
  mutate(date = date(datetime))

#### Old method  of creating data

# DateSeriesWY2023 <- data.frame(date = seq(as.Date(start.date),as.Date(end.date), by = "1 days"))
# date.key = DateSeriesWY2023

#### Clean up data and make sure not too many dates missing -------------------------------

OBI.fnu.smelt <- OBI.fnu %>%
  select(date, parameter_value) %>% rename(OBI.fnu.smelt = parameter_value) %>%
  pad #double check all dates in there

(OBI.fnu.smelt %>% filter(is.na(OBI.fnu.smelt))) # 5 days missing
 
FPT.cfs.smelt <- FPT.cfs %>% 
  select(date, parameter_value) %>% rename(FPT.cfs.smelt = parameter_value) %>%
  pad %>%
  arrange(date) %>%
  mutate(FPT.cfs.smelt = as.numeric(FPT.cfs.smelt),
         FPT.3day.cfs = rollapplyr(FPT.cfs.smelt,3,  mean, align = "right", partial =T)) %>%
  filter(date >= start.date)

(FPT.cfs.smelt %>% filter(is.na(FPT.cfs.smelt))) # 12 days missing

FPT.fnu.smelt <- FPT.fnu %>%
  select(date, parameter_value) %>% rename(FPT.fnu.smelt = parameter_value) %>%
  pad %>%
  arrange(date)%>%
  mutate(FPT.fnu.smelt = as.numeric(FPT.fnu.smelt),
         FPT.3day.fnu = rollapplyr(FPT.fnu.smelt,3, mean, align = "right", partial =TRUE)) %>%
  filter(date >= start.date)

(FPT.fnu.smelt %>% filter(is.na(FPT.fnu.smelt))) # 26 days missing

CLC.F.smelt <- clc.C %>% 
  select(date, parameter_value) %>% rename(CLC.C.smelt = parameter_value) %>%
  mutate(CLC.F.smelt = (CLC.C.smelt * 9/5) + 32) %>%
  pad

(CLC.F.smelt %>% filter(is.na(CLC.F.smelt))) # 0 days missing

MSD.F.salmon <- MSD.f %>%
  group_by(date) %>% 
  mutate(msd.F = mean(parameter_value,na.rm =TRUE)) %>% 
  ungroup() %>%
  select(date, msd.F) %>% 
  distinct() %>% 
  drop_na() %>%
  pad() %>%
  arrange(date) 

PPT.F.salmon <- PPT.f %>%
  group_by(date) %>% 
  mutate(ppt.F = mean(parameter_value,na.rm =TRUE)) %>% 
  ungroup() %>%
  select(date, ppt.F) %>% 
  distinct() %>% 
  drop_na() %>%
  pad() %>%
  arrange(date) 

SJJ.C.smelt <- SJJ.f %>%
  group_by(date) %>% 
  mutate(sjj.F = mean(parameter_value,na.rm =TRUE)) %>% 
  ungroup() %>%
  select(date, sjj.F) %>% 
  distinct() %>% 
  drop_na() %>%
  pad() %>%
  arrange(date) %>%
  mutate(sjj.C = (sjj.F -32) * 5/9,
         sjj.C.3day = rollapplyr(sjj.C,3,  mean, align = "right", partial =T)) %>%
  select(date, sjj.C.3day)
  

# Combine into one df and write ----------------------------------
smelt_env_params <- reduce(list(OBI.fnu.smelt, FPT.cfs.smelt, FPT.fnu.smelt, SJJ.C.smelt), dplyr::left_join, by = "date")
# write_csv(smelt_env_params, "EnvironmentalConditions/output/Data_smelt_environmental.csv")

offramp_env_params <- reduce(list(CLC.F.smelt, MSD.F.salmon, PPT.F.salmon), dplyr::left_join, by = "date") %>%
  filter(date>="2023-06-01")
 # write_csv(offramp_env_params, "EnvironmentalConditions/output/Offramp_temperatures_smelt_salmon.csv")

# Make plots -----------------------------------------

theme_plots <- theme(axis.title.x = element_blank(),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 12))
#uncomment once qwest data is received 
 (plot_qwest <- ggplot(qwest) + 
  geom_hline(yintercept = 0,  linewidth = 1, linetype = "dashed", color = "gray70") +
   geom_line(aes(Date, QWESTcfs)) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
   labs(y = "QWEST (cfs)") +
  theme_bw() +
   theme_plots)

(plot_sjj <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 12,  linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, sjj.C.3day)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    labs(y = "SJJ Temperature (°C)") +
    theme_bw() +
    theme_plots)

(plot_obi <- ggplot(smelt_env_params) + 
   geom_hline(yintercept = 12,  linewidth = 1, linetype = "dashed", color = "gray70") +
  geom_line(aes(date, OBI.fnu.smelt)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(y = "OBI Turbidity (FNU)") +
  theme_bw() +
    theme_plots)

(plot_fpt1 <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 25000, linewidth = 1, linetype = "dashed", color = "gray70") +
  geom_line(aes(date, FPT.cfs.smelt)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    geom_vline(xintercept = as.numeric(as.Date("2024-01-23")), 
               color = "red") +
    geom_vline(xintercept = as.numeric(as.Date("2024-02-05")), 
               color = "red") +
    labs(y = "FPT Flow (cfs)", title = "A") +
  theme_bw() +
  theme_plots)

(plot_fpt2 <- ggplot(smelt_env_params) + 
    geom_hline(yintercept = 50, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, FPT.fnu.smelt)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_vline(xintercept = as.numeric(as.Date("2024-01-23")), 
               color = "red") +
    geom_vline(xintercept = as.numeric(as.Date("2024-02-05")), 
               color = "red") +
    labs(y = "FPT Turbidity (FNU)", title = "B") +
    theme_bw() +
    theme_plots)

(plot_fpt <- plot_fpt1/plot_fpt2)


(plot_clc <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 77, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, CLC.F.smelt)) +
    labs(y = "CLC Temperature (°C)", title = "C") +
    theme_bw() +
    theme_plots)

(plot_msd <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 71.6, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, msd.F)) +
    labs(y = "MSD Temperature (°F)", title = "A") +
    theme_bw() +
    theme_plots)

(plot_ppt <- ggplot(offramp_env_params) + 
    geom_hline(yintercept = 71.6, linewidth = 1, linetype = "dashed", color = "gray70") +
    geom_line(aes(date, ppt.F)) +
    labs(y = "PPT Temperature (°F)", title = "B") +
    theme_bw() +
    theme_plots)

(plot_offramp <- plot_msd/plot_ppt/plot_clc)



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
