library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(deltamapr)
library(readxl)
library(ggspatial)
library(lubridate)
library(viridis)


# TFCF: 37.815176 -121.560709 (WGS84)
# Skinner: 37.82524 -121.59523

# Compile Stations 
sta_20mm <- read_csv("DeltaSmelt/data/CDFW 20mm station gps csv file.csv") %>%
  mutate(Source = "20-mm")
sta_sls <- read_csv("DeltaSmelt/data/CDFW 20mm station gps csv file.csv") %>%
  mutate(Source = "SLS")
sta_skt <- read_csv("DeltaSmelt/data/SKTstations.csv") %>%
  select(-1, -Description) %>%
  mutate(Longitude = Longitude * -1) %>%
  rename(Source = Survey)
sta_salvage <- data.frame(Source = c("CVP Salvage", "SWP Salvage"),
                          Station = c("TFCF", "Skinner"),
                          Latitude = c(37.815176,37.82524),
                          Longitude = c(-121.560709, -121.59523))
sta_broodstock <- data.frame(Source  = "Broodstock",
                             Station = "Broodstock",
                             Latitude = 38.0636,
                             Longitude = -121.7986) 
sta_chipps <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.11&entityid=99a038d691f27cd306ff93fdcbc03b77") %>%
  filter(MethodCode == "MWTR" & Location == "Chipps Island") %>%
  mutate(Source = "Chipps") %>%
  rename(Station = StationCode) %>%
  select(Source, Station, Latitude, Longitude)

sta_all <- rbind(sta_20mm, sta_sls, sta_skt, sta_salvage, sta_broodstock, sta_chipps)
sta_all_sf <- sta_all %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

skt_sf <- sta_all_sf %>%
  filter(Source == "SKT")
sls_sf <- sta_all_sf %>%
  filter(Source == "SLS")

releases <- read_excel("DeltaSmelt/data/release_locations_dates_2023.xlsx")
releases_sf <- releases %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%

  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

# Read in fish data ----------------------------------------------
# Broodstock fish: 38 03' 816" N 121 47, 915" W (from Luke)

# Just SLS and 20mm here
data_cdfw <- read_excel(here::here("DeltaSmelt/data/CDFW_DS_Catch_2023.xlsx")) %>%
  mutate(Source = Gear,
         Gear = ifelse(Source == "SKT", "Kodiak", "20mm"),
         Station = as.character(Station)) %>%
  select(SampleDate, Source, Gear, Station, Catch) %>%
  filter(Source!="SKT") %>%
  distinct()

# Salvage, Broodstock, Chipps, SKT. From Kristi & my spreadsheet
data_salvage_broodstock_chipps <- read_excel(here::here("DeltaSmelt/data/Other_DS_Catch_2023.xlsx")) %>%
  rename(SampleDate = Date) %>%
  mutate(Gear = "Kodiak",
         Catch = 1,
         Station = as.character(Station)) %>%
  select(SampleDate, Source, Gear, Station, Catch, Mark)

# EDSM
data_edsm <- read_excel(here::here("DeltaSmelt/data/USFWS_DS_Catch_2023.xlsx")) %>%
  mutate(Date = mdy(Date),Source = "EDSM") %>%
  rename(SampleDate = Date,
         Station = `Station Code`,
         Gear = Method) %>%
  select(SampleDate, Source, Gear, Station, Catch, Mark, Latitude = `Start Latitude`, Longitude = `Start Longitude`)

smelt1 <- bind_rows(data_salvage_broodstock_chipps, data_cdfw) %>%
  left_join(sta_all)


allsmelt <- bind_rows(data_edsm, smelt1) 
allsmelt_sf <- allsmelt %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta))

sum(allsmelt$Catch)

adult <- allsmelt_sf %>% filter(Gear == "Kodiak") 
larval <- allsmelt_sf %>% filter(Gear == "20mm") #missing one larva - come back and check


# Create map

(map_detections_a <- ggplot() + 
    geom_sf(data = WW_Delta, color = "darkslategray3") +
    geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.5,inherit.aes = FALSE)+
     geom_sf(data = releases_sf, shape = 18, size =2.5,  color = "red", inherit.aes = FALSE) + 
    geom_sf(data = adult, aes(shape = Source), size =2.5,  inherit.aes = FALSE) + 
   
    geom_sf_text(data = skt_sf, mapping = aes(label = Station), size = 3, nudge_x = -0.012, nudge_y = 0.016) +
    annotation_north_arrow(location = "tl", which_north = "true",
                                pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                                style = north_arrow_fancy_orienteering) +
        annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
    scale_x_continuous(limits = c(-122.35, -121.3)) + 
    scale_y_continuous(limits = c(37.8, 38.6)) +
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) + 
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.text = element_text(size = 8)))

(map_detections_l <- ggplot() + 
    geom_sf(data = WW_Delta, color = "darkslategray3") +
    geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.5,inherit.aes = FALSE)+
    geom_sf(data = releases_sf, shape = 18, size =2.5,  color = "red", inherit.aes = FALSE) + 
    geom_sf(data = larval, aes(shape = Source), size =2.5,  inherit.aes = FALSE) + 
    geom_sf_text(data = sls_sf, mapping = aes(label = Station), size = 3, nudge_x = -0.012, nudge_y = 0.016) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_x_continuous(limits = c(-122.35, -121.3)) + 
    scale_y_continuous(limits = c(37.8, 38.6)) +
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) + 
    guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.text = element_text(size = 9)))

adult_releases <- bind_rows(adult, releases_sf) %>%
  mutate(Release = if_else(!is.na(Event), "Release Event", "NA"),
         Mark2 = if_else(grepl("VIE", Mark) | grepl("None", Mark) | grepl("AdClipped", Mark), Mark, paste0("VIE-", Mark))) %>%
  mutate(Release_Event = case_when(Mark2 == "AdClipped" ~ "1/18 Trailer", 
                             Mark2 == "VIE-LOA" ~ "1/25 Hard",
                             Mark2 == "VIE-RBP" ~ "11/30 Soft",
                             Mark2 == "VIE-ROP" ~ "1/19 Hard",
                             Mark2 == "VIE-LRA" ~ "11/30 Hard",
                             Mark2 == "VIE-RGP" ~ "1/26 Soft",
                             Mark2 == "None" ~ "Unmarked")) %>%
  mutate(Release_Event = factor(Release_Event, levels = c("11/30 Hard", "11/30 Soft", "1/18 Trailer", "1/19 Hard", 
                                                          "1/25 Hard", "1/26 Soft", "Unmarked")))
adult_only <- adult_releases %>%  filter(Release == "NA")
releases_only <- adult_releases %>% filter(Release == "Release Event") %>%
  mutate(ReleaseLabel = paste0(Release_Event, " Release")) %>%
  mutate(ReleaseLabel = factor(ReleaseLabel, levels = c("11/30 Hard Release", "11/30 Soft Release", "1/18 Trailer Release", "1/19 Hard Release", 
                                                          "1/25 Hard Release", "1/26 Soft Release")))

(map_detections <- ggplot() + 
    geom_sf(data = WW_Delta, color = "gray70", fill = "gray90", alpha = 0.5) +
    # geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), alpha = 0.1,inherit.aes = FALSE)+
    geom_sf(data = releases_only, shape = 9, size =6,  aes(color = ReleaseLabel), inherit.aes = FALSE) + 
    geom_sf(data = adult_releases, aes(fill = Release_Event), shape = 21, size = 3.5, alpha = 0.75, color = "black", inherit.aes = FALSE) + 
    geom_sf_text(data = adult_releases, mapping = aes(label = Event), size = 4.5, nudge_x = -0.014, nudge_y = 0.017) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_fill_manual(values = c(viridis(6, option = "turbo"), "gray50")) + 
    viridis::scale_color_viridis(option = "turbo", discrete = TRUE) +
    # scale_shape_manual(values = c(21, 9)) +
    scale_size_manual(values = c(3, 6)) +
    scale_x_continuous(limits = c(-122.2, -121.4)) + 
    scale_y_continuous(limits = c(37.8, 38.4)) +
    guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.text = element_text(size = 11)))
# Write maps ------------------------------------

tiff("DeltaSmelt/output/Figure_map_adultDS.tiff", width = 8, height = 8.5, units = "in", res = 300, compression = "lzw")
map_detections_a
dev.off()

tiff("DeltaSmelt/output/Figure_map_ljuvDS.tiff", width = 7.5, height = 8.5, units = "in", res = 300, compression = "lzw")
map_detections_l
dev.off()

tiff("DeltaSmelt/output/Figure_map_releases.tiff", width = 8.5, height = 8.5, units = "in", res = 300, compression = "lzw")
map_detections
dev.off()
