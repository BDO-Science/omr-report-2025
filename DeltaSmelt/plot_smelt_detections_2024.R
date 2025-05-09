library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(deltamapr)
library(readxl)
library(ggspatial)
library(lubridate)
library(viridis)

# Maps ------------------
# TFCF: 37.815176 -121.560709 (WGS84)
# Skinner: 37.82524 -121.59523

## Compile Stations ------------
sta_20mm <- read_csv("DeltaSmelt/data/CDFW 20mm station gps csv file.csv") %>%
  mutate(Source = "20-mm")
sta_sls <- read_csv("DeltaSmelt/data/CDFW 20mm station gps csv file.csv") %>%
  mutate(Source = "SLS")
sta_salvage <- data.frame(Source = c("CVP Salvage", "SWP Salvage"),
                          Station = c("TFCF", "Skinner"),
                          Latitude = c(37.815176,37.82524),
                          Longitude = c(-121.560709, -121.59523))
sta_all <- rbind(sta_20mm, sta_sls, sta_salvage)
sta_all_sf <- sta_all %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

skt_sf <- sta_all_sf %>%
  filter(Source == "SKT")
sls_sf <- sta_all_sf %>%
  filter(Source == "SLS")

releases <- read_excel("DeltaSmelt/data/release_locations_dates_2024.xlsx") %>%
  mutate(Release = case_when(Event == "LS1" ~ "11/15 LS Hard",
                             Event == "LS2" ~ "1/10 LS Hard",
                             Event == "2a" ~ "12/19 Hard",
                             Event == "2b" ~ "12/20 Trailer",
                             Event == "3a" ~ "1/24 Hard",
                             Event == "3b" ~ "1/25 Soft",
                             Event == "4a" ~ "1/31 Trailer",
                             Event == "4b" ~ "1/30 Soft",
                             Event == "1a" ~ "12/12 Hard",
                             Event == "1b" ~ "12/14 Soft"))%>%
  mutate(Release = factor(Release, levels = c("11/15 LS Hard","12/12 Hard", 
                                              "12/14 Soft","12/19 Hard", "12/20 Trailer", 
                                              "1/10 LS Hard","1/24 Hard",
                                              "1/25 Soft","1/30 Soft","1/31 Trailer")))%>%
  mutate(Mark = if_else(Mark == "Ad", "AdClipped", paste0("VIE-", Mark)))

releases_sf <- releases %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%

  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

## Read in fish data ----------------------------------------------

# Just SLS and 20mm here
data_20mm <- read_excel(here::here("DeltaSmelt/data/CDFW_20mm_20241204.xlsx")) %>%
  mutate(Source = "20-mm",
         Gear = "20mm",
         Station = as.character(Station),
         Catch =1,
         LifeStage = if_else(Length>19, "Juvenile", if_else(Length>58, "Adult", "Larva"))) %>%
  select(SampleDate, Source, Gear, Station, LifeStage, Catch) %>%
  left_join(sta_all)

# Salvage Data. From SMT spreadsheet
data_salvage <- read_excel(here::here("DeltaSmelt/data/Salvage_2024.xlsx")) %>%
  filter(Source == "Salvage") %>%
  mutate(Station = as.character(Station),
         Station = replace(Station, Station == "Tracy", "TFCF"),
         Source = replace(Source, Source == "Salvage", "CVP Salvage"),
         Gear = "Salvage") %>%
  select(SampleDate, Source, Gear, Station, LifeStage, Catch, Mark) %>%
  left_join(sta_all)

# USFWS 
data_usfws <- read_excel(here::here("DeltaSmelt/data/USFWS_DSM_Catch_WY2024_20240711.xlsx"), 
                         sheet = 2) %>%
  mutate(Station = as.character(StationCode)) %>%
  select(SampleDate, Source=Survey, Gear, Station, 
         LifeStage,Catch, Mark=MarkCode, 
         Latitude= LatitudeTarget, Longitude=LongitudeTarget)

## USFWS Adult (Kodiak)
data_usfwsA <- data_usfws %>%
filter(Gear%in% c("Kodiak", "E-fishing"))  %>%
  select(SampleDate, Source, Gear, Station, LifeStage,Catch, Mark, Latitude, Longitude)

## USFWS Larvae/Juveniles (20mm Phase 2)
data_usfwsL <- read_excel(here::here("DeltaSmelt/data/USFWS_DSM_Catch_WY2024_20240815.xlsx"), 
                          sheet = 2) %>%
  filter(Gear == "20mm") %>%
  mutate(Station = as.character(StationCode)) %>%
  select(SampleDate, Source=Survey, Gear, Station, 
         LifeStage,Catch, Mark=MarkCode, 
         Latitude= LatitudeTarget, Longitude=LongitudeTarget)

allsmelt <- bind_rows(data_salvage, data_usfwsA, data_20mm, data_usfwsL) %>%
  group_by(SampleDate, Source, Gear, Station, LifeStage, Mark, Latitude, Longitude) %>%
  summarize(Catch = sum(Catch, na.rm = TRUE)) %>%
  ungroup()

allsmelt_sf <- allsmelt %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

sum(allsmelt$Catch)

# Separate out datasets for adult vs larval/juvenile
adult <- allsmelt_sf %>% 
  filter(Gear %in% c("Kodiak", "E-fishing")|(Source == "CVP Salvage" & LifeStage == "Adult"))  %>%
  group_by(Station, Source) %>%
  summarize(totalCatch = sum(Catch))
adult_mark <- allsmelt_sf %>%
  filter(Gear %in% c("Kodiak", "E-fishing")|(Source == "CVP Salvage" & LifeStage == "Adult"))  %>%
  group_by(Station, Source, Mark) %>%
  summarize(totalCatch = sum(Catch))

larval <- allsmelt_sf %>% 
  filter(Gear == "20mm" | (Source == "CVP Salvage" & LifeStage != "Adult")) %>%
  group_by(Station, Source, LifeStage) %>%
  summarize(totalCatch = sum(Catch))

larval %>%
  group_by(LifeStage) %>%
  summarize(total = sum(totalCatch))
sum(adult$totalCatch)


mark <- allsmelt%>%
  group_by(Gear, LifeStage, Mark) %>%
  summarize(total = sum(Catch))
## Create maps ----------------------------

# Adult
(map_detections_a <- ggplot() + 
    geom_sf(data = WW_Delta, color = "darkslategray3") +
    geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.4,inherit.aes = FALSE)+
    geom_sf(data = adult, aes(shape = Source, size = totalCatch),  inherit.aes = FALSE) + 
    geom_sf(data = releases_sf, shape = 23, size =3,  fill = "red", color = "black", inherit.aes = FALSE) + 
    geom_sf_text(data = skt_sf, mapping = aes(label = Station), size = 3, nudge_x = -0.012, nudge_y = 0.016) +
    annotation_north_arrow(location = "tl", which_north = "true",
                                pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                                style = north_arrow_fancy_orienteering) +
        annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_x_continuous(limits = c(-122.35, -121.3)) + 
    scale_y_continuous(limits = c(37.8, 38.6)) +
    scale_shape_manual(values = c(17, 16, 14))+
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) + 
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.box = "vertical",
          legend.text = element_text(size = 8)))

# Larval
(map_detections_l <- ggplot() + 
    geom_sf(data = WW_Delta, color = "darkslategray3") +
    geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.4,inherit.aes = FALSE)+
    geom_sf(data = larval, aes(shape = Source, size = totalCatch),   inherit.aes = FALSE) + 
    geom_sf(data = releases_sf, shape = 23, size =3,  fill = "red", color = "black", inherit.aes = FALSE) + 
    # geom_sf_text(data = sls_sf, mapping = aes(label = Station), size = 3, nudge_x = -0.012, nudge_y = 0.016) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_x_continuous(limits = c(-122.35, -121.3)) + 
    scale_y_continuous(limits = c(37.8, 38.6)) +
    scale_shape_manual(values = c(16, 17, 14))+
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) + 
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.box = "vertical",
          legend.text = element_text(size = 9)))

# Releases

adult_releases <- left_join(adult_mark, releases) %>%
  rename(Release_Event = Release) %>%
  mutate(Release_Event = if_else(is.na(Release_Event), "Not tagged", Release_Event)) %>%
  mutate(Release = if_else(!is.na(Event), "Release Event", "Not tagged")) %>%
  mutate(Release_Event = factor(Release_Event, levels = c("11/15 LS Hard","12/12 Hard", 
                                              "12/14 Soft", "1/10 LS Hard","1/24 Hard",
                                              "1/25 Soft","1/30 Soft","1/31 Trailer", "Not tagged")))
       
(map_detections <- ggplot() + 
    geom_sf(data = WW_Delta, color = "gray60", fill = "gray90", alpha = 0.5) +
    geom_sf(data = releases_sf, shape = 9, size =6, color = "maroon",  inherit.aes = FALSE) + 
    geom_sf(data = adult_releases, aes(fill = Release_Event), shape = 21, size = 3.5, alpha = 0.75, color = "black", inherit.aes = FALSE) + 
    annotate(geom = "text", y = 38.16956, x = -121.75,  label = "Release Site", size = 4.25) +
    # geom_sf_text(data = releases_sf, label = "Release site", size = 4.5, nudge_x = -0.016, nudge_y = 0.02) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_fill_manual(values = c(viridis(8, option = "turbo"), "gray50")) + 
    viridis::scale_color_viridis(option = "turbo", discrete = TRUE) +
    # scale_shape_manual(values = c(21, 9)) +
    scale_size_manual(values = c(3, 6)) +
    scale_x_continuous(limits = c(-122.2, -121.4)) + 
    scale_y_continuous(limits = c(37.8, 38.4)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.text = element_text(size = 11)))

## Write maps ------------------------------------
tiff("DeltaSmelt/output/Figure_map_adultDS.tiff", width = 7, height = 7.5, units = "in", res = 300, compression = "lzw")
map_detections_a
dev.off()

tiff("DeltaSmelt/output/Figure_map_ljuvDS.tiff", width = 7.8, height = 8.5, units = "in", res = 300, compression = "lzw")
map_detections_l
dev.off()

tiff("DeltaSmelt/output/Figure_map_releases.tiff", width = 8.5, height = 8.5, units = "in", res = 300, compression = "lzw")
map_detections
dev.off()

# Region/life stage plots ----------------------------------------------

# Had to revise the groupings to distinguish Hypomesus from Delta Smelt. Can remove the mutate(Group....) lines 
# in subsequent years if this issues does not exist

allsmelt_NAD <- st_transform(allsmelt_sf, crs = st_crs(R_EDSM_Regions_1718P1))
smelt_region <- st_join(allsmelt_NAD, R_EDSM_Regions_1718P1) %>%
  mutate(Stage = if_else(LifeStage %in% c("Adult","Adult (cultured)"), "Adult", 
                         if_else(LifeStage == "Larva", "Juveniles/Larvae",
                                 if_else(LifeStage == "Juvenile" & Gear == "Kodiak", "Adult", "Juveniles/Larvae")))) %>%
  mutate(Region = if_else(Source == "CVP Salvage", "Salvage", Region)) %>%
  mutate(Week = week(SampleDate)) %>%
  mutate(Group = case_when(Stage == "Adult" ~ "Adult",
                           Stage == "Juveniles/Larvae" & Source == "USFWS EDSM" ~ "Juvenile/Larvae Hypomesus",
                           Stage == "Juveniles/Larvae" & (Source == "20-mm" | Source == "CVP Salvage") ~ "Juvenile/Larvae Delta Smelt"))

smelt_region_totals <- smelt_region %>%
  sf::st_drop_geometry() %>%
  group_by(Week)%>%
  mutate(Date = first(SampleDate)) %>%
  ungroup() %>%
  group_by(Week, Date, Group, Region) %>%
  summarize(Total = sum(Catch)) %>%
  ungroup() 
  

tiff("DeltaSmelt/output/Figure_Catch_over_time_2024.tiff", width = 6.3, height = 8, units = "in", res = 300, compression = "lzw")
ggplot(smelt_region_totals) + 
  geom_col(aes(Date, Total, fill = Region), color = "black") +
  facet_wrap(Group~., nrow = 3, scales = "free") +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_fill_viridis(option = "viridis", discrete = TRUE) + 
  labs(y = "Catch") +
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        plot.margin = margin(10, 20, 10, 10))
dev.off()

# Salvage plot -------------------------------

# Copied from Nicole's code. Data adapted from Kyle's salvage report.
salvage <- read_csv("DeltaSmelt/data/Salvage_OMRI_2024.csv") %>%
  janitor::clean_names() %>%
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
