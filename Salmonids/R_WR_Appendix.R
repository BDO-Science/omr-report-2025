library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(here)
library(janitor)

# Code for updating Winter Run Appendix for OMR Report

report_year = 2025

# Escapement -------------------------------------

## Download data
url_escapement <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=All&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"
escapement <- read_csv(url_escapement) %>%
  clean_names() %>%
  rename(Year = end_year_of_monitoring_period) %>%
  filter(!is.na(population_estimate)) %>%
  mutate(Year2 = as.numeric(substr(Year, start = 1, stop = 4)),
         Year = factor(Year2)) %>%
  filter(Year2 > report_year -10)

## Make plot
(plot_escapement <- ggplot(escapement) + 
  geom_col(aes(Year, population_estimate), fill = "steelblue4") +
    geom_text(aes(Year, population_estimate +300, label = population_estimate), size = 4.5) + 
  geom_hline(yintercept = mean(escapement$population_estimate), linetype = "dashed") + 
  labs(y = "Escapement", x = "Brood Year")+
  theme_bw() +
    theme(axis.text = element_text(size = 11),
          axis.title = element_text(size = 12)))

## Write plot
tiff("Salmonids/appendix_outputs/Figure_escapement.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
plot_escapement
dev.off()

# JPI ------------------------------
# JPI spreadsheet data from JPE letter. 
# USFWS (Bill Poytress) will eventually update some of the values from 2022 on.

jpi <- read_csv(here("Salmonids/data/JPI_2002_2024.csv")) %>%
  clean_names() %>%
  filter(by <= report_year) %>%
  rename(jpi = fry_equivalent_jpi,
         etf_survival = etf_survival_rate_percent) %>%
  select(by, jpi, etf_survival) %>%
  mutate(jpi = jpi/1000000,
         jpi_lab = round(jpi, 2),
         etf_survival_lab = round(etf_survival)) %>%
  filter(by > report_year - 11) %>%
  mutate(by = factor(by)) 

(plot_jpi <- ggplot(jpi) + 
    geom_col(aes(by, jpi), fill = "palegreen4", alpha = 0.8, width = 0.8) +
    geom_text(aes(by,jpi+0.15, label = jpi_lab), size = 4.5) + 
    geom_hline(yintercept = mean(jpi$jpi), linetype = "dashed") + 
    labs(y = "Juvenile Production Index (millions)") +
    scale_y_continuous() + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          axis.title.x = element_blank()))

## Write plot
tiff("Salmonids/appendix_outputs/Figure_wr_jpi.tiff", width = 8, height = 6, units = "in", res = 300, compression = "lzw")
plot_jpi
dev.off()

# TDM and ETF --------------------------------------
# TDM Data from CVTEMP

tdm <- read.csv(here("Salmonids/data/ETF_TDM_2002_2024.csv")) %>%
  mutate(unexplained_mortality = 100-ETF_Survival-TDM_NOAA_percent) %>%
  rename(ETF_survival = ETF_Survival) %>%
  filter(Brood.Year > report_year-11) %>%
  mutate(Brood.Year = factor(Brood.Year))%>%
  mutate(color = case_when(Sac.Val.Year.Type == "C" ~ "#D55E00",
                           Sac.Val.Year.Type == "D" ~ "#E69F00",
                           Sac.Val.Year.Type == "AN" ~ "#009E73",
                           Sac.Val.Year.Type == "BN" ~  "black",
                           Sac.Val.Year.Type == "W" ~ "#0072B2")) %>%
  mutate(Brood.Year.Type = paste0(Brood.Year, " (", Sac.Val.Year.Type, ")" )) 

tdm_long <- tdm %>%
  select(Brood.Year.Type, color,
         `Temperature Attributed Mortality` = TDM_NOAA_percent, 
         `Egg-to-Fry Survival` = ETF_survival, 
         `Unattributed Mortality` = unexplained_mortality) %>%
  pivot_longer(cols = `Temperature Attributed Mortality`:`Unattributed Mortality`, names_to = "Fate", values_to = "Percent") %>%
  mutate(Percent_label = round(Percent)) 

yrcolors <- rev(tdm$color)

(plot_tdm_only<- ggplot(tdm) + 
    geom_col(aes(Brood.Year, TDM_NOAA_percent), fill = "steelblue", alpha = 0.8) +
    geom_text(aes(Brood.Year, TDM_NOAA_percent +2, label = round(TDM_NOAA_percent)), size =  4) + 
    geom_hline(yintercept = mean(tdm$TDM_NOAA_percent), linetype = "dashed") + 
    labs(y = "Temperature Dependent\n Mortality (%)", x = "Brood Year") +
    # scale_y_continuous(expand = c(0,0)) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          axis.title.x = element_blank()))

(plot_tdm <- ggplot(tdm_long, aes(Brood.Year.Type, Percent, fill = Fate)) + 
    geom_col(width = 0.65, alpha = 0.9) +
    geom_text(aes(label = Percent_label), position = position_stack(vjust = 0.5), size = 5) +
    scale_fill_manual(values = c("goldenrod","steelblue" ,"gray70")) + 
    labs(x = "Brood Year") + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12, colour = yrcolors),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          legend.position = "bottom",
          legend.title = element_blank()))

(plot_etf <- ggplot(jpi) + 
    geom_col(aes(by, etf_survival), fill = "goldenrod", alpha = 0.8, width = 0.8) +
    geom_text(aes(by, etf_survival +2, label = etf_survival_lab), size =  4) + 
    geom_hline(yintercept = mean(jpi$etf_survival), linetype = "dashed") + 
    labs(y = "Egg-to-Fry Survival (%)") +
    # scale_y_continuous(expand = c(0,0)) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          axis.title.x = element_blank()))

## Write plot
tiff("Salmonids/appendix_outputs/Figure_wr_tdm.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
plot_tdm
dev.off()

tiff("Salmonids/appendix_outputs/Figure_wr_tdm_only.tiff", width = 7, height = 4, units = "in", res = 300, compression = "lzw")
plot_tdm_only
dev.off()

tiff("Salmonids/appendix_outputs/Figure_wr_etf.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
plot_etf
dev.off()

# Hatchery Survival ---------------------------------------------
## This data comes from CalFishTrack. https://oceanview.pfeg.noaa.gov/CalFishTrack/pageLSWR_2025.html
## Tables 3.2 and 3.3 - manually added info to spreadsheet.
## Read in data. Add Brood year as a variable to match other plots. 
hatchery <- readxl::read_excel(here::here("Salmonids/data/HatcheryWinterRunSurvival.xlsx")) %>%
  mutate(BY = factor(BY),
         Metric_label = case_when(Metric == "Benicia" ~ "Minimum Survival to Benicia Bridge East Span (95% CI)",
                            Metric == "Delta" ~ "Minimum Through-Delta Survival (95% CI)",
                            Metric == 'Entry' ~ 'Minimum Survival to Delta Entry (95% CI'))

## Separate out data
hatchery_benicia <- hatchery %>% filter(Metric == "Benicia")
hatchery_delta <- hatchery %>% filter(Metric == "Delta")
hatchery_entry <- hatchery %>% filter(Metric == 'Entry')

## Benicia Plot
ben <- ggplot(data = hatchery_benicia) + 
  geom_point(aes(x = BY, y = Survival)) +
  geom_errorbar(aes(x = BY, ymin = `95LCI`, ymax = `95UCI`), width = 0.1)+
  geom_hline(aes(yintercept = mean(Survival)), linetype = "dashed", color = "maroon")+
  labs(title = paste0("C) ",hatchery_benicia$Metric_label), x = "Brood Year", y  = "Survival (%)")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_text(size = 12))

## Delta Plot
delta <- ggplot(hatchery_delta) + 
  geom_point(aes(BY, Survival)) +
  geom_errorbar(aes(x = BY, ymin = `95LCI`, ymax = `95UCI`), width = 0.1)+
  geom_hline(aes(yintercept = mean(Survival)), linetype = "dashed", color = "navy")+
  labs(title = paste0("B) ", hatchery_delta$Metric_label), x = "Brood Year", y  = "Survival (%)")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_text(size = 12))
mean(hatchery_benicia$Survival)
mean(hatchery_delta$Survival)

##
entry <- ggplot(data = hatchery_entry) + 
  geom_point(aes(x = BY, y = Survival)) +
  geom_errorbar(aes(x = BY, ymin = `95LCI`, ymax = `95UCI`), width = 0.1)+
  geom_hline(aes(yintercept = mean(Survival)), linetype = "dashed", color = "darkorange")+
  labs(title = paste0("A) ",hatchery_entry$Metric_label), x = "Brood Year", y  = "Survival (%)")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_text(size = 12))

## Combine plots
library(patchwork) 
(survival_plot <- entry/ delta / ben)

## Write plot
tiff("Salmonids/appendix_outputs/Figure_wr_hatcherysurvival.tiff", width = 6, height =7, units = "in", res = 300, compression = "lzw")
survival_plot
dev.off()

# Loss trends ---------------------------------------------
## Majority of loss data queried from SacPAS.
## Historic genetic WR loss dataset came from DWR
## Read in data. Add Brood year as a variable to match other plots. 
library(busdater)
wday <- readRDS("salmonids/data/waterDay.rds")

#genetic wr
wr_loss_import <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=yes&age=no') %>%
  janitor::clean_names()

wr_loss_2020_on <- wr_loss_import %>%
  filter(dna_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  mutate(wy = get_fy(date, opt_fy_start = '10-01')) %>%
  group_by(date, wy) %>%
  summarize(loss = sum(loss)) %>%
  ungroup()

wr_loss_pre_2020 <- read_csv("Salmonids/data/genetic_wr_loss_all.csv") %>%
  mutate(date = mdy(`Sample Date`),
         wy = WaterYear) %>%
  group_by(date, wy) %>%
  summarize(loss = sum(Loss)) %>%
  ungroup() %>%
  filter(wy < 2020)

wr_loss_genetic <- bind_rows(wr_loss_2020_on, wr_loss_pre_2020) %>%
  group_by(wy) %>%
  mutate(cumul = cumsum(loss)) %>%
  mutate(wday = wday(date))

wr_loss_genetic_2025 <- wr_loss_genetic %>%
  filter(wy == 2025)
wr_loss_genetic_historic <- wr_loss_genetic %>%
  filter(wy != 2025)
max_loss <- max(wr_loss_genetic_2025$cumul)

wr_loss_genetic_labels <- wr_loss_genetic %>%
  group_by(wy) %>%
  summarize(wday = max(wday),
            cumul = max(cumul)) %>%
  filter(cumul >= max_loss)

wr_loss_anti_labels <- wr_loss_genetic %>%
  group_by(wy) %>%
  summarize(wday = max(wday),
            cumul = max(cumul)) %>%
  filter(cumul < max_loss) %>%
  pull(wy)
text_wys <- paste0("WYs with less total Loss:\n", toString(wr_loss_anti_labels))
natural_spaghetti <- ggplot() +
  geom_line(wr_loss_genetic_historic, mapping = aes(x = wday, y = cumul, group = factor(wy)), linewidth = 1,
            color = 'grey', alpha = 0.5) +
  geom_line(wr_loss_genetic_2025, mapping = aes(x = wday, y = cumul), linewidth = 1.5,
            color = 'black') +
  labs(x = 'Date', y = 'Cumulative Loss',
       title = paste0("A) Genetic Winter-run Cumulative Loss")) +
  geom_text(wr_loss_genetic_labels, mapping = aes(x = wday + 4, y = cumul+10, label = factor(wy)), size = 3) +
  #annotate(geom = 'text', 90, y = 1400, label = text_wys) +
  scale_x_continuous(breaks = c(61, 92, 123, 153, 183, 213),
                     labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May')) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())
natural_spaghetti

#hatchery wr
wr_hatchery_import <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3At&dnaOnly=no&age=no') %>%
  janitor::clean_names()

wr_hatchery_loss <- wr_hatchery_import %>%
  filter(cwt_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  mutate(wy = get_fy(date, opt_fy_start = '10-01')) %>%
  group_by(date, wy) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  group_by(wy) %>%
  mutate(cumul = cumsum(loss),
         wday = wday(date))
  
wr_loss_hatchery_2025 <- wr_hatchery_loss %>%
  filter(wy == 2025)
wr_loss_hatchery_historic <- wr_hatchery_loss %>%
  filter(wy != 2025)
max_loss_hatch <- max(wr_loss_hatchery_2025$cumul)
wr_loss_hatchery_labels <- wr_hatchery_loss %>%
  group_by(wy) %>%
  summarize(wday = max(wday),
            cumul = max(cumul)) %>%
  filter(cumul >= max_loss_hatch)

wr_hatch_loss_anti_labels <- wr_hatchery_loss %>%
  group_by(wy) %>%
  summarize(wday = max(wday),
            cumul = max(cumul)) %>%
  filter(cumul < max_loss_hatch) %>%
  pull(wy)
text_wys_hatch <- paste0("WYs with less total Loss:\n", toString(wr_hatch_loss_anti_labels))

hatchery_spaghetti <- ggplot() +
  geom_line(wr_loss_hatchery_historic, mapping = aes(x = wday, y = cumul, group = factor(wy)), linewidth = 1,
            color = 'grey', alpha = 0.5) +
  geom_line(wr_loss_hatchery_2025, mapping = aes(x = wday, y = cumul), linewidth = 1.5,
            color = 'black') +
  labs(x = 'Date', y = 'Cumulative Loss',
       title = paste0("B) Hatchery Winter-run Cumulative Loss")) +
  geom_text(wr_loss_hatchery_labels, mapping = aes(x = wday + 4, y = cumul+10, label = factor(wy)), size = 3) +
  #annotate(geom = 'text', 153, y = 1400, label = text_wys_hatch) +
  scale_x_continuous(breaks = c(61, 92, 123, 153, 183, 213),
                     labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May'),
                     limits = c(61,213)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_text(size = 12))
hatchery_spaghetti

cumulative_plots <- natural_spaghetti/hatchery_spaghetti

tiff("Salmonids/appendix_outputs/Figure_wr_cumulative_historic.tiff", width = 8, height =8, units = "in", res = 300, compression = "lzw")
cumulative_plots
dev.off()