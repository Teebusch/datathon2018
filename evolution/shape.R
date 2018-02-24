
library(lubridate)
library(geosphere)
library(zoo)
require(tidyverse)
require(ggmap)
require(Imap)
#require(plyr)
require(scales)
library(ggalt)
library("viridis")
source("evolution/multiplot.R")


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
  mutate(  bin_half = round(time_rel/30), idp = paste0(qid, " (", id, ")") ) %>%
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
  mutate(  bin_half = round(time_rel/30) ) %>%
  select(id, bin_half, speed, power, cadence, heartrate) %>%
  group_by(id, bin_half) %>%
  summarise(speed = mean(speed, na.rm = TRUE), speed_sd = sd(speed, na.rm = TRUE),
            power = mean(power, na.rm = TRUE), power_sd = sd(power, na.rm = TRUE),
            cadence = mean(cadence, na.rm = TRUE), cadence_sd = sd(cadence, na.rm = TRUE),
            heartrate = mean(heartrate, na.rm = TRUE), cadence_sd = sd(heartrate,na.rm = TRUE))



df1_progress_selection = merge(df1_progress, df1_selection_bin) %>% group_by(id) %>% arrange(bin_half) %>% ungroup()
# ----------------------------------------------------------


df1_distance = df1_progress_selection %>%
  group_by(bin_half) %>%
  mutate(lat_mean = mean(latitude),
         long_mean = mean(longitude),
         distance = gdist(lon.1 = longitude, lat.1 = latitude,
                          lon.2 = long_mean, lat.2 = lat_mean,
                          units="m")) %>%
  summarise(distance = sum(distance, na.rm = TRUE)) %>%
  mutate(lag_bin_half = lag(bin_half),
         lag_bin_half = ifelse(is.na(lag_bin_half), bin_half, lag_bin_half),
         diff_bins = ifelse(bin_half - lag_bin_half > 1, 1, 0),
         check = cumsum(diff_bins)) %>%
  ungroup() %>%
  mutate(bin_plot= bin_half / 2 )



df1_distance %>%
  ggplot(aes(x = bin_plot, y = distance, group = check)) +
  geom_area() +
  stat_xspline(spline_shape=0) +
  scale_x_continuous(breaks = seq(0, 274, 3) ) +
  xlab("Time (minutes)") +
  ylab("Distance (meters)") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Distance between cyclists (2017-10-17 UTC)") +
  ggsave("evolution/img/Distance between cyclists (2017-10-17 UTC).png")

# Export object
save(df1_distance, file = "evolution/positions/area_plot1.RData")

# 
# # Aaaaaah
# race_progress %>% 
#   filter(half %in% sequence ) %>%
#   group_by(half) %>%
#   summarise(lat = max(latitude) - min(latitude),
#             long = max(longitude) - min(longitude)) %>%
#   ungroup() %>%
#   summarise(lat = max(lat), long = max(long))
# # Aaaaaaaah
# 
# structure = function(i) {
#   race_structure = race_progress %>% 
#     filter(half == sequence[i]) %>%
#     mutate(lat = latitude - min(latitude),
#            long = longitude - min(longitude)) 
#   
#   find_hull <- function(df) df[chull(df$lat, df$long), ]
#   hulls <- plyr::ddply(race_structure, "half", find_hull)
#   
#   p = race_structure %>% 
#     ggplot(aes(lat,long, group = half)) + 
#     scale_x_continuous(limits = c(0,0.004461299)) +
#     scale_y_continuous(limits = c(0,0.004766111)) +
#     geom_point(size = 0.25) + 
#     geom_polygon(data = hulls, alpha = 0.75) + 
#     theme_bw() +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank(),
#           axis.title.y=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks.y=element_blank()) + 
#     ggtitle(paste0("Time: ",sequence[i]))
#   
#   # print(c(min(race_structure$lat), max(race_structure$lat)))
#   # print(c(min(race_structure$long), max(race_structure$long)))
#   return(p)
# }
# 
# p1 = structure(1)
# p2 = structure(2)
# p3 = structure(3)
# p4 = structure(4)
# p5 = structure(5)
# p6 = structure(6)
# p7 = structure(7)
# p8 = structure(8)
# p9 = structure(9)
# p10 = structure(10)
# p11 = structure(11)
# p12 = structure(12)
# p13 = structure(13)
# p14 = structure(14)
# p15 = structure(15)
# 
# 
# multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, cols=4)