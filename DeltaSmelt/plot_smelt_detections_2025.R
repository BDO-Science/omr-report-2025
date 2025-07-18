library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(deltamapr)
library(readxl)
library(ggspatial)
library(lubridate)
library(viridis)
library(janitor)
library(forcats)

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

release_info <- read_excel("DeltaSmelt/data/Releases_2025.xlsx") %>%
  clean_names(case = "upper_camel")

releases_sf <- release_info %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

## Read in fish data ----------------------------------------------

# Juvenile EDSM 
data_edsmL <- read_excel(here::here("DeltaSmelt/data/EDSM_LarJuv_2025.xlsx"))

# Larval - Salvage, 20-mm (but these were all Hypomesus sp. this year)
data_otherL <- read_excel(here::here("DeltaSmelt/data/Other_LarJuv_2025.xlsx")) %>%
  left_join(sta_all) %>%
  mutate(Source = if_else(Source == "CVP Salvage", "TFCF", Source))

# Adult EDSM (from USFWS's running DS spreadsheet)
data_adult <- read_excel(here::here("DeltaSmelt/data/USFWS_Adult_20250624_KS.xlsx"), sheet = 2) %>%
  filter(SampleDate > ymd("2024-10-01")) %>%
  mutate(Catch = 1) %>%
  select(SampleDate, Source=Survey, Gear=MethodCode, Station=StationCode,
         LifeStage, Catch, Mark=MarkCode, Latitude = LatitudeStart, Longitude = LongitudeStart) 

# Combine smelt 
# For now, don't include larval IDs since they are Hypomesus sp.
allsmelt <- bind_rows(data_otherL, data_edsmL, data_adult) %>%
  group_by(SampleDate, Source, Gear, Station, LifeStage, Mark, Latitude, Longitude) %>%
  summarize(Catch = sum(Catch, na.rm = TRUE)) %>%
  ungroup()

allsmelt_sf <- allsmelt %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

# Separate out datasets for adult vs larval/juvenile
adult <- allsmelt_sf %>% 
  filter(SampleDate < ymd("2025-04-01"))  %>%
  group_by(Station, Source) %>%
  summarize(totalCatch = sum(Catch))

larjuv <- allsmelt_sf %>% 
  filter(SampleDate > ymd("2025-04-01"))  %>%
  group_by(Station, Source) %>%
  summarize(totalCatch = sum(Catch))

adult_mark <- allsmelt_sf %>%
  filter(SampleDate < ymd("2025-06-01"))  %>%
  group_by(Station, Source, Mark) %>%
  summarize(totalCatch = sum(Catch)) %>%
  ungroup() %>%
  left_join(release_info %>% select(Mark, MarkCode, ReleaseDate, ReleaseSite)) %>%
  mutate(ReleaseSite = case_when(ReleaseSite == "Lookout Slough" ~ "LS",
                                  ReleaseSite == "Rio Vista" ~ "RV")) %>%
  mutate(MarkCode = replace(MarkCode, is.na(MarkCode), "None")) %>%
  mutate(Release = paste0(ReleaseDate, ReleaseSite))

mark <- allsmelt%>%
  group_by(Gear, LifeStage, Mark) %>%
  summarize(total = sum(Catch))

## Create maps ----------------------------

# Adult
(map_detections_a <- ggplot() + 
    geom_sf(data = WW_Delta, color = "darkslategray3") +
    geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.4,inherit.aes = FALSE)+
    geom_sf(data = releases_sf, shape = 23, size =3,  fill = "red", color = "black",  inherit.aes = FALSE) + 
    geom_sf(data = adult, aes(shape = Source, size = totalCatch),  inherit.aes = FALSE, show.legend = "point") + 
    annotation_north_arrow(location = "tl", which_north = "true",
                                pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                                style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_x_continuous(limits = c(-122.35, -121.3)) + 
    scale_y_continuous(limits = c(37.8, 38.6)) +
    scale_shape_manual(values = c(20, 6, 17, 12, 4))+
    scale_size(range = c(2,7), breaks = c(1, 2, 3,4,5,6)) + 
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) + 
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top",
          legend.box = "vertical",
          legend.title = element_blank(),
          legend.text = element_text(size = 9)))

# Juvenile
(map_detections_lj <- ggplot() + 
    geom_sf(data = WW_Delta, color = "darkslategray3") +
    geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.4,inherit.aes = FALSE)+
    geom_sf(data = releases_sf, shape = 23, size =3,  fill = "red", color = "black", inherit.aes = FALSE) + 
    geom_sf(data = larjuv, aes(shape = Source, size = totalCatch),   inherit.aes = FALSE) + 
    # geom_sf_text(data = sls_sf, mapping = aes(label = Station), size = 3, nudge_x = -0.012, nudge_y = 0.016) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_x_continuous(limits = c(-122.35, -121.3)) + 
    scale_y_continuous(limits = c(37.8, 38.6)) +
    scale_shape_manual(values = c(15, 17, 4))+
    scale_size(range = c(2, 3), breaks = c(1,2)) + 
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) + 
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top",
          legend.box = "vertical",
          legend.title = element_blank(),
          legend.text = element_text(size = 9)))

# Releases

adult_releases <- adult_mark%>%
  mutate(Release_Event = Release) %>%
  mutate(Release_Event = if_else(Release_Event=="NANA", "Not tagged", Release_Event)) %>%
  mutate(Release_Event = as.factor(Release_Event),
         Release_Event = fct_shift(Release_Event, 4),
         Release_Event = fct_relevel(Release_Event, "Not tagged", after =Inf))
       
(map_detections <- ggplot() + 
    geom_sf(data = WW_Delta, color = "gray60", fill = "gray90", alpha = 0.5) +
    geom_sf(data = releases_sf, shape = 9, size =6, color = "red",  inherit.aes = FALSE) + 
    geom_sf(data = adult_releases, aes(fill = Release_Event), shape = 21, size = 3.5, alpha = 0.75, color = "black", inherit.aes = FALSE) + 
    annotate(geom = "text", y = 38.16956, x = -121.76,  label = " Rio Vista (RV)\n Release", size = 4.25) +
    annotate(geom = "text", y = 38.34, x = -121.78,  label = "Lookout Slough (LS)\n Release", size = 4.25)+
    # geom_sf_text(data = releases_sf, label = "Release site", size = 4.5, nudge_x = -0.016, nudge_y = 0.02) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_fill_manual(values = c(viridis(7, option = "turbo"), "gray50")) + 
    viridis::scale_color_viridis(option = "turbo", discrete = TRUE) +
    # scale_shape_manual(values = c(21, 9)) +
    scale_size_manual(values = c(3, 6)) +
    scale_x_continuous(limits = c(-122.2, -121.4)) + 
    scale_y_continuous(limits = c(37.8, 38.4)) +
    # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.text = element_text(size = 11)))

## Write maps ------------------------------------
tiff("DeltaSmelt/output/Figure_map_adultDS.tiff", width = 7.8, height = 7.5, units = "in", res = 300, compression = "lzw")
map_detections_a
dev.off()

tiff("DeltaSmelt/output/Figure_map_ljuvDS.tiff", width = 7.8, height = 7.5, units = "in", res = 300, compression = "lzw")
map_detections_lj
dev.off()

tiff("DeltaSmelt/output/Figure_map_releases.tiff", width = 8.5, height = 8.5, units = "in", res = 300, compression = "lzw")
map_detections
dev.off()

# Region/life stage plots ----------------------------------------------

allsmelt_NAD <- st_transform(allsmelt_sf, crs = st_crs(R_EDSM_Regions_1718P1))
smelt_region <- st_join(allsmelt_NAD, R_EDSM_Regions_1718P1) %>%
  mutate(Region = if_else(Source == "TFCF", "Salvage", Region)) %>%
  mutate(Week = week(SampleDate))  %>%
  mutate(LifeStage = if_else(SampleDate < ymd("2025-06-01"), "Adult/SubAdult", "Juvenile"))

smelt_region_totals <- smelt_region %>%
  sf::st_drop_geometry() %>%
  group_by(Week)%>%
  mutate(Date = first(SampleDate),
         Region = as.factor(Region)) %>%
  ungroup() %>%
  group_by(Week, Date, LifeStage, Region) %>%
  summarize(Total = sum(Catch)) %>%
  ungroup() 
  

tiff("DeltaSmelt/output/Figure_Catch_over_time_2025.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
ggplot(smelt_region_totals) + 
  geom_col(aes(Date, Total, fill = Region), color = "black") +
  # facet_wrap(LifeStage~., nrow = 2, scales = "free") +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_fill_viridis(option = "viridis", discrete = TRUE) + 
  labs(y = "Catch") +
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        plot.margin = margin(10, 20, 10, 10))
dev.off()


