

library(lubridate)
library(geosphere)
library(zoo)
require(tidyverse)
require(ggmap)
require(Imap)
#require(plyr)
require(scales)
#library(ggalt)
library("viridis")



# Loading data
df = read_rds("cycling.rds") %>% 
  group_by(day) %>%
  mutate(time_rel = ts - min(ts),
         bin_half = round(time_rel/30),
         bin_min = round(time_rel)/60) %>%
  ungroup()


# ---------------------------------------------
# I focus only in the race 
# Participants Lat: 10,12,4,8,9,6,7
# Participants with Power: 10, 12,4,8, 9
df1 = df %>% filter(day == "2017-10-17 UTC") 

# Difference between riders
select_order = function(distance) {
  return (order(distance))
}

# Percentage completed
df1_progress  = df1 %>% 
  mutate(  bin_half = round(time_rel/10), idp = paste0(qid, " (", id, ")") ) %>%
  filter(bin_min < 275) %>%
  group_by(bin_half) %>%
  arrange(time_rel) %>%
  mutate(latitude_ref = mean(latitude), 
         longitude_ref = mean(longitude)) %>%
  ungroup() %>%
  group_by(id, idp, bin_half) %>%
  summarise(latitude = mean(latitude), 
            longitude = mean(longitude),
            latitude_ref = mean(latitude_ref), 
            longitude_ref = mean(longitude_ref)
  ) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(latitude_ref = lead(latitude_ref), 
         longitude_ref = lead(longitude_ref),
         lag_bin_half = lag(bin_half),
         lag_bin_half = ifelse(is.na(lag_bin_half), bin_half, lag_bin_half),
         diff_bins = ifelse(bin_half - lag_bin_half > 1, 1, 0),
         check = cumsum(diff_bins))  %>%
  ungroup() %>%
  filter(!is.na(latitude_ref) & !is.na(longitude_ref)) %>%
  mutate( distance = gdist(lon.1 = longitude, 
                           lat.1 = latitude, 
                           lon.2 = longitude_ref, 
                           lat.2 = latitude_ref, 
                           units="m")) %>%
  group_by(bin_half) %>%
  mutate(order = select_order(distance) ) %>% 
  ungroup()


# ----------------------------------------------------------
# Adding additional variables
df1_selection_bin  = df1 %>% 
  mutate(  bin_half = round(time_rel/10) ) %>%
  select(id, bin_half, speed, power, cadence, heartrate) %>%
  group_by(id, bin_half) %>%
  summarise(speed = mean(speed, na.rm = TRUE), speed_sd = sd(speed, na.rm = TRUE),
            power = mean(power, na.rm = TRUE), power_sd = sd(power, na.rm = TRUE),
            cadence = mean(cadence, na.rm = TRUE), cadence_sd = sd(cadence, na.rm = TRUE),
            heartrate = mean(heartrate, na.rm = TRUE), cadence_sd = sd(heartrate,na.rm = TRUE))



df1_progress_selection = merge(df1_progress, df1_selection_bin) %>% group_by(id) %>% arrange(bin_half) %>% ungroup()
# ----------------------------------------------------------

df1_plot = df1_progress_selection %>%
  group_by(bin_half) %>%
  mutate(lat = latitude - min(latitude), long = longitude - min(longitude)) %>%
  ungroup() %>%
  mutate(bin_half = bin_half/6)

hulls <- df1_plot %>% select(idp, lat, long, bin_half) %>%
  group_by(bin_half) %>% 
  do(.[chull(.[2:3]), ])

# ggplot(df1_plot, aes(x = long, y = lat)) +
#   geom_polygon(data = hulls, alpha = 0.1) +
# ggsave("evolution/img/position.png")

# Loading plot
load("area_plot1.RData")
#load("evolution/positions/area_plot1.RData")
# area_plot1 = df1_distance %>%
#   ggplot(aes(x = bin_plot, y = distance, group = check)) +
#   geom_area() +
#   stat_xspline(spline_shape=0) +
#   scale_x_continuous(breaks = seq(0, 274, 3) ) +
#   xlab("Time (minutes)") +
#   ylab("Distance (meters)") + 
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   ggtitle("Distance between cyclists (2017-10-17 UTC)")

