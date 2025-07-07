library(tidyverse)
library(busdater)
library(janitor)
library(zoo)
library(CDECRetrieve)

#############################
#current WY loss data/figures
#############################

wy <- get_fy(Sys.Date(), opt_fy_start = '10-01')  #pull the water year based on BY designation in LTO docs
jpe <- 98893 #set natural winter-run JPE
jpe_hatch <- 135342 #set hatchery JPE

#pull in winter-run loss data
wrurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=',wy,
                '&species=1%3Aall&dnaOnly=no&age=no')
wr_loss <- read_csv(wrurl) %>%
  clean_names()
write.csv(wr_loss, 'Salmonids/output/wy_2025_wr_loss.csv', row.names = FALSE) #saving to include in data appendix

#pull in and summarize steelhead loss data
shurl <- paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year='
                ,wy,'&species=2%3Af&dnaOnly=no&age=no')
sh_import <- read_csv(shurl) %>%
  clean_names() 

write.csv(sh_import, 'Salmonids/output/wy_2025_sh_loss.csv', row.names = FALSE) #saving to include in data appendix

sh_loss <- sh_import %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  mutate(cumul = cumsum(loss)) %>%
  na.omit()

#summarize winter-run natural and hatchery loss data
wr_natural <- wr_loss %>% 
  filter(adipose_clip == 'Unclipped' &
           dna_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  mutate(cumul = cumsum(loss))

wr_hatchery <- wr_loss %>%
  filter(cwt_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(cumul = cumsum(loss))

#winter-run  weekly distributed loss
wr_thresholds <- read_csv('Salmonids/data/weeklyThresholds.csv') %>% #pulling in weekly distributed loss thresholds
  mutate(StartDate = dmy(paste0(StartDate,'-',wy))) %>% #converting to date format with current water year
  mutate(EndDate = dmy(paste0(EndDate,'-',wy))) %>% #ditto
  rowwise() %>%
  mutate(date = list(seq.Date(StartDate, EndDate, by = "day"))) %>%
  unnest(date) %>%
  select(date, HistoricPresent) %>%
  mutate(threshold = ((jpe*.005)*.5)*HistoricPresent)

wr_weekly <- data.frame(date = seq(as.Date('2024-12-01'), as.Date('2025-06-30'), 1)) %>%
  left_join(wr_natural, by = 'date') %>%
  left_join(wr_thresholds, by = 'date') %>%
  replace(is.na(.), 0) %>%
  mutate(threshold = round(threshold, 2)) %>%
  mutate(sum_7D_loss = rollsum(loss, k = 7, fill = NA, align = 'right')) %>%
  filter(date >= as.Date(paste0(wy,'-01-01')))

#steelhead weekly distributed loss
sh_weekly <- data.frame(date = seq(as.Date('2024-12-01'), as.Date('2025-06-30'), 1)) %>%
  left_join(sh_loss, by = 'date') %>%
  replace(is.na(.), 0) %>%
  mutate(threshold = 120) %>%
  mutate(sum_7D_loss = rollsum(loss, k = 7, fill = NA, align = 'right')) %>%
  filter(date >= as.Date(paste0(wy,'-01-01')))

# 1. Tag & bind your weekly tables ------------------------------

SH_weekly_WY <- sh_weekly %>%
  rename(Date = date) %>%           # unify the date column name
  mutate(species = "Steelhead")

wr_weekly_WY <- wr_weekly %>%
  rename(Date = date) %>%
  mutate(species = "Winter-run")

combined_weekly <- bind_rows(SH_weekly_WY, wr_weekly_WY) %>%
  arrange(species, Date) %>%
  group_by(species) %>%
  mutate(
    cumul_loss = cumsum(loss)       # cumulative loss over the water year
  ) %>%
  ungroup()


# 2. (Optional) hline for Steelhead’s one-time 120 threshold ----

hline_data <- tibble(
  species      = "Steelhead",
  yintercept   = 120
)

# define your common x‐axis window
start_date <- as.Date(paste0(wy, "-01-01"))
end_date   <- as.Date(paste0( wy  , "-06-30"))

# 3. Plot ----------------------------------------------------------

p <- ggplot(combined_weekly) +
  # bars, now filled by facility
  #geom_col(aes(x = Date, y = loss, fill = facility),
           #position = "dodge", alpha = 0.7) +
  geom_line(aes(x = Date, y = sum_7D_loss, color = "weekly loss"), # 7-day rolling sum
            size = 1) +
  geom_line(aes(x = Date, y = threshold, color = "weekly threshold"), # distributed-loss threshold
            linetype = "dotted", size = 1) +
  #geom_line(aes(x = Date, y = cumul_loss, color = "cumulative loss"),   # cumulative loss
            #linetype = "dashed", size = 1) +
  facet_wrap(~ species, scales = "free_y") +
  scale_fill_viridis_d(name = "Facility", option = "viridis") +   # viridis scales
  scale_color_viridis_d(name = "", begin = 0.1, end = 0.5) +
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0,0))
  ) +
  labs(x = NULL, y = "Fish loss") +
  theme_bw() +
  theme(
    # make *all* text bold:
    text         = element_text(face = "bold"),
    # if you need to be extra-sure axis texts are bold:
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    strip.text   = element_text(face = "bold"),  # facet labels
    legend.text  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    # keep the slanted x-labels
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  )

# print to screen
print(p)

# save high-res PNG for Word
ggsave("Salmonids/output/loss_plot.png", plot = p,
       width  = 8,    # inches
       height = 5,    # inches
       dpi    = 300)  # sufficient for print/Word

# Steelhead plot
sh_data <- combined_weekly %>% filter(species == "Steelhead")

p_sh <- ggplot(sh_data) +
  geom_line(aes(x = Date, y = sum_7D_loss), size = 1) +
  geom_line(aes(x = Date, y = threshold),
            linetype = "dotted", size = 1) +
  #geom_hline(aes(yintercept = 120), color = "red", linetype = "dotted", size = 1) +
  #scale_color_viridis_d(name = "", begin = 0.1, end = 0.5) +
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0,0))
  ) +
  labs(title = NULL, x = NULL, y = "# Steelhead") +
  theme_bw(base_size = 14) +
  theme(
    text         = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text   = element_blank(),
    legend.position = "bottom"
  )

# Winter-run plot
wr_data <- combined_weekly %>% filter(species == "Winter-run")

p_wr <- ggplot(wr_data) +
  geom_line(aes(x = Date, y = sum_7D_loss), size = 1) +
  geom_line(aes(x = Date, y = threshold),
            linetype = "dotted", size = 1) +
  #scale_color_viridis_d(name = "", begin = 0.1, end = 0.5) +
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0,0))
  ) +
  labs(title = NULL, x = NULL, y = "# Salmon") +
  theme_bw(base_size = 14) +
  theme(
    text         = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text   = element_blank(),
    legend.position = "bottom"
  )

# Print to screen if you like
print(p_sh)
print(p_wr)

# Save each out as a high-res PNG for Word
ggsave("Salmonids/output/steelhead_weekly_loss.png", p_sh,
       width = 8, height = 5, dpi = 300)
ggsave("Salmonids/output/winterrun_weekly_loss.png", p_wr,
       width = 8, height = 5, dpi = 300)

# 1) Filter for LSNFH hatchery fish and extract date ----------------------------
wr_hatch <- wr_loss %>%
  filter(cwt_hatch == "LSNFH") %>%        # keep only LSNFH releases
  mutate(date = as.Date(sample_time)) %>% # convert datetime → Date
  select(date)

# 2) Tally daily losses & fill in zeros ----------------------------------------
daily_hatch <- wr_hatch %>%
  count(date, name = "daily_loss") %>%    # one row per date with n fish lost
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(daily_loss = 0)) %>%
  arrange(date)

# --- 1. Compute your threshold values --------------------------------
thr100 <- jpe * 0.005
thr75  <- thr100 * 0.75
thr50  <- thr100 * 0.50

threshold_lines <- tibble(
  pct   = c("100 %", "75 %", "50 %"),
  value = c(thr100, thr75, thr50)
)

# --- 2. Prepare your LSNFH‐only daily cumulative series --------------
daily_hatch <- wr_loss %>%
  filter(cwt_hatch == "LSNFH") %>%
  mutate(date = as.Date(sample_time)) %>%
  count(date, name = "daily_loss") %>%
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(daily_loss = 0)) %>%
  arrange(date) %>%
  mutate(cumul_loss = cumsum(daily_loss))

# compute the date limits from your data
date_limits <- range(daily_hatch$date)

fpt_q <- cdec_query('FPT', '20', 'H', '2025-01-01')

p_hatch3b <- ggplot(daily_hatch, aes(x = date)) +
  # 1) daily loss as light grey bars
  geom_col(aes(y = daily_loss),
           fill  = "grey40",
           width = 1,
           alpha = 0.6) +
  geom_line(data = fpt_q, x=) +
  # 2) cumulative loss line
  geom_line(aes(y = cumul_loss),
            size  = 1.2) +
  
  # 3) percent‐of‐JPE thresholds
  geom_hline(data = threshold_lines,
             aes(yintercept = value, linetype = pct),
             size = 1) +
  scale_linetype_manual(
    name   = "% Threshold",
    values = c("100 %" = "dashed",
               "75 %"  = "dotted",
               "50 %"  = "dotdash")
  ) +
  
  # 4) x‐axis ticks
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0, 0))
  ) +
  
  labs(
    x = NULL,
    y = "# Salmon"
  ) +
  theme_bw(base_size = 14) +
  theme(
    text           = element_text(face = "bold"),
    axis.text.x    = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  )

print(p_hatch3b)

# And save for your Word doc:
ggsave("Salmonids/output/wr_hatch_daily_and_cumul.png",
       plot = p_hatch3b,
       width  = 8, height = 5, dpi = 300)

fpt_q2 <- fpt_q %>%
  mutate(
    date        = as.Date(datetime),
    # scale so that max(flow_scaled) == max_thresh
    flow_scaled = parameter_value * max_thresh / max_flow
  )

# 0) re-compute your maxima
max_loss     <- max(daily_hatch$cumul_loss,      na.rm=TRUE)
max_thresh   <- max(threshold_lines$value)        # thr100,75,50
max_flow     <- max(fpt_q$parameter_value,       na.rm=TRUE)

upper_y      <- max(max_loss, max_thresh) * 1.05

# 1) rebuild the plot, swapping in the new limits
p_hatch <- ggplot(daily_hatch, aes(x = date)) +
  # bars + cum-loss + FPT flow (same as before) …
  geom_col(aes(y = daily_loss), fill="grey40", width=1, alpha=0.6) +
  geom_line(aes(y = cumul_loss), size=1.2) +
  geom_line(data = fpt_q2,
            aes(x = date, y = flow_scaled),
            size=1, color="grey80", linetype="twodash") +
  
  # your three %-of-JPE hlines
  geom_hline(data = threshold_lines,
             aes(yintercept = value, linetype = pct),
             size = 1) +
  scale_linetype_manual(
    name   = "% Threshold",
    values = c("100 %"="dashed", "75 %"="dotted", "50 %"="dotdash")
  ) +
  
  # *** new y-scale limits ***
  scale_y_continuous(
    name     = "# Salmon",
    limits   = c(0, max_thresh * 1.05),
    sec.axis = sec_axis(~ . * (max_flow / max_thresh),
                        name = "Flow (cfs)")
  ) +
  
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand      = expansion(add = c(0, 0))
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 14) +
  theme(
    text           = element_text(face = "bold"),
    axis.text.x    = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  )

print(p_hatch)

# 3) Save for Word --------------------------------------------------------------
ggsave("Salmonids/output/wr_hatch_with_flow.png",
       plot = p_hatch,
       width  = 8, height = 5, dpi = 300)

# 1. Set up Steelhead annual thresholds ----------------------------------------
sh_thr100 <- 3000
sh_thr75  <- sh_thr100 * 0.75
sh_thr50  <- sh_thr100 * 0.50

sh_thresh_lines <- tibble(
  pct   = c("100 %", "75 %",   "50 %"),
  value = c(sh_thr100, sh_thr75, sh_thr50)
)

# 2. Build the daily + cumulative series --------------------------------------
steel_daily <- sh_loss %>%
  # make sure your date column is Date class
  mutate(date = as.Date(date)) %>%
  # daily total loss (in case you had multiple entries per day)
  group_by(date) %>%
  summarise(daily_loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
  # fill in any missing dates with zeros
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(daily_loss = 0)) %>%
  arrange(date) %>%
  # running total
  mutate(cumul_loss = cumsum(daily_loss))

# 3. Plot with three percent‐lines --------------------------------------------
p_sh <- ggplot(steel_daily, aes(x = date, y = cumul_loss)) +
  # main cumulative‐loss line
  geom_line(size = 1.2) +
  
  # percent‐of‐annual hlines
  geom_hline(data = sh_thresh_lines,
             aes(yintercept = value, linetype = pct),
             size = 1) +
  
  scale_linetype_manual(
    name   = "% Threshold",
    values = c("100 %" = "dashed",
               "75 %"  = "dotted",
               "50 %"  = "dotdash")
  ) +
  
  # weekly ticks on x‐axis
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0, 0))
  ) +
  
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = "# Steelhead"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title      = element_text(size = 16),
    plot.subtitle   = element_text(size = 12, face = "italic"),
    legend.position = "bottom"
  )

print(p_sh)

# 4. Save high-res PNG for Word import ----------------------------------------
ggsave("Salmonids/output/steelhead_cumul_loss.png",
       plot = p_sh,
       width  = 8,   # inches
       height = 5,   # inches
       dpi    = 300)

p_sh2 <- ggplot(steel_daily, aes(x = date)) +
  # daily loss as grey bars
  geom_col(aes(y = daily_loss),
           fill   = "grey40",
           width  = 1,
           alpha  = 0.6) +
  # cumulative‐loss line on top
  geom_line(aes(y = cumul_loss),
            size   = 1.2) +
  # percent‐of‐annual hlines
  geom_hline(data = sh_thresh_lines,
             aes(yintercept = value, linetype = pct),
             size = 1) +
  scale_linetype_manual(
    name   = "% Threshold",
    values = c("100 %" = "dashed",
               "75 %"  = "dotted",
               "50 %"  = "dotdash")
  ) +
  # weekly x‐axis ticks
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "2 weeks",        # one tick every 7 days
    date_labels = "%b %d",
    expand      = expansion(add = c(0, 0))
  ) +
  labs(
    y = "# Steelhead",
    x = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  )

print(p_sh2)

# Save if you like
ggsave("Salmonids/output/steelhead_daily_and_cumul_loss.png",
       plot = p_sh2,
       width  = 8, height = 5, dpi = 300)

###########################
#historical loss comparison
###########################

###genetic winter-run by month
CVP <- read_csv('Salmonids/data/CVP_genetics.csv')
SWP <- read_csv('Salmonids/data/SWP_genetics.csv')

wr_temp <- wr_loss %>% 
  filter(adipose_clip == 'Unclipped' &
           dna_race == 'Winter') %>%
  mutate(date = as.Date(sample_time)) %>%
  group_by(date) %>%
  summarize(n = sum(nfish)) %>%
  ungroup() %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(n = sum(n)) %>%
  mutate(class = 'WY 2025')

wr_by_month <- bind_rows(CVP, SWP) %>%
  mutate(month = month(SampleDateTime, label = TRUE),
         wy = get_fy(SampleDateTime, opt_fy_start = '07-01')) %>%
  filter(Genetic_Assignment == 'Winter') %>%
  group_by(month) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(class = 'Historic (2011-2024)') %>%
  bind_rows(wr_temp) %>%
  filter(month %in% c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May')) %>%
  mutate(month = factor(month, levels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May'))) %>%
  group_by(class) %>%
  mutate(prop = prop.table(n))

wr_month_graph <- wr_by_month %>%
  ggplot(aes(x = month, y = prop*100, fill = class)) +
  geom_col(color = 'black', position = 'dodge') +
  scale_fill_viridis_d() +
  labs(y='Percent of Loss') +
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )
wr_month_graph
###historic steelhead
sh_import_all_years <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Af&dnaOnly=no&age=no') %>%
  clean_names()

sh_by_month <- sh_import_all_years %>%
  mutate(date = as.Date(sample_time)) %>%
  mutate(class = if_else(date >= as.Date('2024-07-01'), 'WY 2025', 'Historic (2009-2024)'),
         month = month(date, label = TRUE),
         wy = get_fy(date, opt_fy_start = '07-01')) %>%
  filter(wy > 2008) %>%
  group_by(month, class) %>%
  summarize(loss = sum(loss)) %>%
  ungroup() %>%
  na.omit() %>%
  group_by(class) %>%
  mutate(prop = prop.table(loss)) %>%
  mutate(month = factor(month, levels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 
                                          'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 
                                          'May', 'Jun')))
sh_month_graph <- sh_by_month %>%
  ggplot(aes(x = month, y = prop*100, fill = class)) +
  geom_col(color = 'black', position = 'dodge') +
  scale_fill_viridis_d() +
  labs(y='Percent of Loss') +
  theme_bw(base_size = 14) +
  theme(
    text            = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )
sh_month_graph

ggsave(sh_month_graph, file = 'Salmonids/output/sh_loss_by_month.png', width = 8, height = 5)
ggsave(wr_month_graph, file = 'Salmonids/output/wr_loss_by_month.png', width = 8, height = 5)
######################
#spring-run surrogates
######################

#######scrapping SacPAS surrogate stuff
library(rvest)
library(janitor)

hatcheryurl <- 'https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY2025/cwt_spring_surrogates.html'
webpage <- read_html(hatcheryurl)
tables <- webpage %>%
  html_nodes("table")

surrogates <- html_table(tables[[1]]) %>%
  select(1,3:12) %>%
  select(-7,-10,-11) %>%
  mutate('Percent of Threshold' = 
           paste0(round(`Confirmed Loss`/`Loss Threshold (0.25% of CWT Released)` * 100,1), "%"))

write.csv(filter(surrogates, Type == 'Yearling'), file = 'Salmonids/output/SR_yearling_surrogates.csv', row.names = FALSE)
write.csv(filter(surrogates, Type != 'Yearling'), file = 'Salmonids/output/SR_yoy_surrogates.csv', row.names = FALSE)