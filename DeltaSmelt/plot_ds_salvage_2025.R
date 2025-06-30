library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(viridis)
library(janitor)

# Salvage plot -------------------------------

# Copied from Nicole's LFS code. Data adapted from Kyle's salvage report.
salvage <- read_csv("DeltaSmelt/data/Salvage_OMRI_2024.csv") %>%
  clean_names() %>%
  mutate(date = mdy(date),
         federal_season_salvage_adult = as.numeric(federal_season_salvage_adult),
         federal_season_salvage_juvenile = as.numeric(federal_season_salvage_juvenile),
         OMR = gsub(middle_old_r_net_daily_flow_cfs,pattern = ",", replacement = ""),
         OMR = replace(OMR, OMR == "ND", NA),
         OMR = as.numeric(OMR))

(sal <- ggplot() +
    geom_line(data=salvage, aes(x=date, y=federal_season_salvage_adult), color="navy", linewidth=1) +
    geom_line(data=salvage, aes(x=date, y=federal_season_salvage_juvenile), color="lightblue3", linewidth=1, position=position_dodge(width=0.2)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    annotate(geom = "text", label = "Juvenile Salvage", x= as.Date("2024-05-24"), y = 7)+
    annotate(geom = "text", label = "Adult Salvage", x= as.Date("2024-03-16"), y = 53)+
    theme_bw() +
    ylab("DS Cumulative Seasonal Salvage") +
    xlab("Date") +
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_blank()) )

#plot flow
(flow <- ggplot() +
    geom_line(data=salvage, aes(x=date, y= OMR), linewidth=1) +
    theme_bw() +
    labs(y = "Middle + Old River net daily flow (cfs)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")+
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_blank()) )

library(ggpubr)
figure <- ggarrange(sal, flow,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure

ggsave("DeltaSmelt/output/2024salvage.png", height=7, width=8, units="in")