

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
source("evolution/multiplot.R")


# Loading data
df12 = read_rds("cycling.rds") %>% 
  group_by(day) %>%
  mutate(time_rel = ts - min(ts),
         bin_half = round(time_rel/30),
         bin_min = round(time_rel)/60) %>%
  ungroup() %>%
  filter(bin_min > 273 & bin_min < 275 & day == "2017-10-17 UTC")

#  Time vs Power
df12 %>%
  ggplot(aes(x = dt, y = power, group = id)) +
  geom_point(size=0.3) +
  facet_wrap(  ~ id) + 
  ggtitle("Time vs. Power") +
  ggsave("evolution/img/evolution-d1-last/Time vs. Power.png")

# Time vs Latitude
df12 %>%
  ggplot(aes(x = dt, y = latitude, group = id)) +
  geom_point() +
  facet_wrap(  ~ id) + 
  ggtitle("Time vs. Laitude") +
  ggsave("evolution/img/evolution-d1-last/Time vs. Latitude.png")


# ---------------------------------------------
# I focus only in the race 
# Participants Lat: 10,12,4,8,9,6,7
# Participants with Power: 10, 12,4,8, 9

# Latitude and Longitude for day 2017-10-17 UTC
df12 %>%
  ggplot(aes(x = longitude, y = latitude, color = dt)) +
  geom_path(show.legend = TRUE) + 
  facet_wrap(~id) +
  scale_color_viridis(NULL, option = "C") +
  ggtitle("Latitude and Longitude for day 2017-10-17 UTC")  +
  ggsave("evolution/img/evolution-d1-last/Latitude vs Longitude (Race).png")


read_rds("cycling.rds") %>% 
  group_by(day) %>%
  mutate(time_rel = ts - min(ts),
         bin_half = round(time_rel/30),
         bin_min = round(time_rel)/60) %>%
  ungroup() %>%
  filter(bin_min > 275  & day == "2017-10-17 UTC") %>%
  ggplot(aes(x = longitude, y = latitude, color = dt)) +
  geom_path(show.legend = TRUE) + 
  facet_wrap(~id) +
  scale_color_viridis(NULL, option = "A") +
  ggtitle("Latitude and Longitude for day 2017-10-17 UTC (After)")  +
  ggsave("evolution/img/evolution-d1-last/Latitude vs Longitude (Race) [After].png")



# Difference between riders
select_order = function(distance) {
  return (order(distance))
}

# Percentage completed
df12_progress  = df12 %>% 
  mutate(  bin_half = round(time_rel/7), idp = paste0(qid, " (", id, ")") ) %>%
  filter(bin_min > 246 & bin_min < 276) %>%
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


# Plot
# df12_progress %>%
#   ggplot(aes(x = bin_half, y = order, group = check )) +
#   ggalt::geom_xspline(spline_shape=0.4) +
#   scale_y_reverse(breaks = seq(1:7), labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th")) + 
#   scale_x_continuous(breaks = seq(0, 274, 3) ) +
#   xlab("Time (minutes)") +
#   ylab("") + 
#   theme_bw() +
#   facet_grid(idp ~ .) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   ggtitle("Evolution of the team (2017-10-17 UTC)") +
#   ggsave("evolution/img/evolution-d1-last/Evolution of the team.png")

# ----------------------------------------------------------
# Adding additional variables
df12_selection_bin  = df12 %>% 
  mutate(  bin_half = round(time_rel/7) ) %>%
  select(id, bin_half, speed, power, cadence, heartrate) %>%
  group_by(id, bin_half) %>%
  summarise(speed = mean(speed, na.rm = TRUE), speed_sd = sd(speed, na.rm = TRUE),
            power = mean(power, na.rm = TRUE), power_sd = sd(power, na.rm = TRUE),
            cadence = mean(cadence, na.rm = TRUE), cadence_sd = sd(cadence, na.rm = TRUE),
            heartrate = mean(heartrate, na.rm = TRUE), cadence_sd = sd(heartrate,na.rm = TRUE))



df12_progress_selection = merge(df12_progress, df12_selection_bin) %>%
  group_by(id) %>% 
  arrange(bin_half) %>% 
  ungroup() %>%
  mutate(bin_half = bin_half / 8.6)
  
# ----------------------------------------------------------

# Speed
df12_progress_selection %>%
  ggplot(aes(x = bin_half, y = order, group = check, color = speed )) +
  geom_line() +
  scale_color_viridis(NULL, option = "C") +
  scale_y_reverse(breaks = seq(1:7), labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th")) + 
  scale_x_continuous(breaks = seq(0, 276, 0.1) ) +
  xlab("Time (minutes)") +
  ylab("") + 
  theme_bw() +
  facet_grid(idp ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  guides(fill=guide_legend(title="Speed")) +
  ggtitle("Evolution of the team by Speed (2017-10-17 UTC)") +
  ggsave("evolution/img/evolution-d1-last/Evolution of the team by Speed.png")

# Power
df12_progress_selection %>%
  ggplot(aes(x = bin_half, y = order, group = check, color = power )) +
  geom_line() +
  scale_color_viridis(NULL, option = "C") +
  scale_y_reverse(breaks = seq(1:7), labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th")) + 
  scale_x_continuous(breaks = seq(0, 276, 0.1) ) +
  xlab("Time (minutes)") +
  ylab("") + 
  theme_bw() +
  facet_grid(idp ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  guides(fill=guide_legend(title="Power")) +
  ggtitle("Evolution of the team by Power (2017-10-17 UTC)") +
  ggsave("evolution/img/evolution-d1-last/Evolution of the team by Power.png")

# Cadence
df12_progress_selection %>%
  ggplot(aes(x = bin_half, y = order, group = check, color = cadence )) +
  geom_line() +
  scale_color_viridis(NULL, option = "C") +
  scale_y_reverse(breaks = seq(1:7), labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th")) + 
  scale_x_continuous(breaks = seq(0, 276, 0.1) ) +
  xlab("Time (minutes)") +
  ylab("") + 
  theme_bw() +
  facet_grid(idp ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  guides(fill=guide_legend(title="Cadence")) +
  ggtitle("Evolution of the team by Cadence (2017-10-17 UTC)") +
  ggsave("evolution/img/evolution-d1-last/Evolution of the team by Cadence.png")

# Heart rate
df12_progress_selection %>%
  ggplot(aes(x = bin_half, y = order, group = check, color = heartrate )) +
  geom_line() +
  scale_color_viridis(NULL, option = "C") +
  scale_y_reverse(breaks = seq(1:7), labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th")) + 
  scale_x_continuous(breaks = seq(0, 276, 0.1) ) +
  xlab("Time (minutes)") +
  ylab("") + 
  theme_bw() +
  facet_grid(idp ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  guides(fill=guide_legend(title="Heart Rate")) +
  ggtitle("Evolution of the team by Heart Rate (2017-10-17 UTC)") +
  ggsave("evolution/img/evolution-d1-last/Evolution of the team by Heart Rate.png")

# 




# sequence = seq(min(df1_progress_selection$bin_half), max(df1_progress_selection$bin_half), 20)
# z_df1_progress_sum = df1_progress_selection %>% filter(bin_half %in% sequence)
# 
# z_df1_progress_sum %>%
#   ggplot(aes(x = bin_half, y = order, group = id )) +
#   geom_line() +
#   geom_point() +
#   scale_y_reverse(breaks = seq(1:7)) + 
#   theme_bw() 