
require(tidyverse)
require(ggmap)
require(Imap)
require(plyr)


# Loading data
load("evolution/data/cycling_clean.RData")
source("evolution/multiplot.R")

race = unique(df %>% select(-EventEnqueuedUtcTime, 
                            -EventProcessedUtcTime))  %>% 
  filter(ts > 1515200000) %>%
  mutate(time = ts - min(df$ts), 
         half = round(time/30),
         power = as.numeric(power)) 

# We remove the two strange riders that doesn't have speed. See the following plot
race_clean = race %>%
  filter( !( qid %in% c("AFQ36860", "AFQ47736")) ) 

# # Save data
# save(race, file = "evolution/data/race.RData")

# Validate 
ggplot(race_clean, aes(x = time, y = power, group = qid)) +
  geom_line() +
  facet_wrap(~qid)

race_clean %>%
  filter(time > 1700 * 60) %>%
ggplot(aes(x = time, y = power, group = qid)) +
  geom_line() +
  facet_wrap(~qid) +
  scale_x_continuous(breaks = seq(1700 * 60, 1800 * 60, 30)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

race_clean %>%
  filter(time < 1500 * 60) %>%
  ggplot(aes(x = time, y = speed, group = qid)) +
  geom_line() +
  facet_wrap(~qid) +
  scale_x_continuous(breaks = seq(0 * 60, 1000 * 60, 30)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Draw the coordinates
race_clean %>%
  group_by(qid) %>%
  arrange(ts) %>%
  ungroup() %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_path(show.legend = FALSE) + 
  facet_wrap(~qid)

race_clean %>%
  filter(time > 1711 * 60) %>%
  group_by(qid) %>%
  arrange(time) %>%
  ungroup() %>%
  ggplot(aes(x = longitude, y = latitude, color = time)) +
  geom_path(show.legend = TRUE) + 
  facet_wrap(~qid)

# Finish line
race_gmaps = race_clean %>% filter(time > 1705.4 * 60 & time < 1706 * 60)

center = paste(min(race_gmaps$latitude) + (max(race_gmaps$latitude) - min(race_gmaps$latitude))/2,
               min(race_gmaps$longitude) + (max(race_gmaps$longitude) - min(race_gmaps$longitude))/2, sep=" ")

gmap <- get_map(location = center, zoom = 16, maptype = "satellite", source = "google")

ggmap(gmap) + 
  geom_path(data = race_gmaps, aes(x = longitude, y = latitude, color = power)) +
  scale_colour_gradient(low = "green", high = "red") +
  facet_wrap(~qid)

# Heading the hotel (Speed)
race_gmaps = race_clean %>% filter(time > 1705.4 * 60 & time < 17011 * 60)

center = paste(min(race_gmaps$latitude) + (max(race_gmaps$latitude) - min(race_gmaps$latitude))/2,
               min(race_gmaps$longitude) + (max(race_gmaps$longitude) - min(race_gmaps$longitude))/2, sep=" ")

gmap <- get_map(location = center, zoom = 15, maptype = "satellite", source = "google")

ggmap(gmap) + 
  geom_path(data = race_gmaps, aes(x = longitude, y = latitude, color = as.numeric(speed)), size = 0.9) +
  scale_colour_gradient(low = "green", high = "red") +
  facet_wrap(~qid)

# Percentage completed
race_distance  = race_clean %>% 
  filter(time < 1706 * 60) %>%
  group_by(qid) %>%
  arrange(time) %>%
  ungroup() %>%
  group_by(qid) %>%
  mutate(latitude_lag = lag(latitude), longitude_lag = lag(longitude),
         latitude_lag = ifelse(is.na(latitude_lag), latitude, latitude_lag),
         longitude_lag = ifelse(is.na(longitude_lag), longitude, longitude_lag),
         distance = gdist(lon.1 = longitude, 
                          lat.1 = latitude, 
                          lon.2 = longitude_lag, 
                          lat.2 = latitude_lag, 
                          units="m"),
         distance_cum = sum(distance),
         speed_cum = sum(as.numeric(speed))) %>%
  ungroup()
  
race_distance %>% group_by(qid) %>%
  summarise(distance_cum = sum(distance) / 1000,
            speed_cum = sum(as.numeric(speed)) / 1000
            )

# Difference between riders

select_order = function(distance) {
  return (order(distance))
}

# Percentage completed
race_progress  = race_clean %>% 
  filter(time < 1706 * 60) %>%
  group_by(half) %>%
  arrange(time) %>%
  mutate(latitude_ref = mean(latitude), longitude_ref = mean(longitude)) %>%
  ungroup() %>%
  group_by(qid, half) %>%
  summarise(latitude = mean(latitude), longitude = mean(longitude),
            latitude_ref = mean(latitude_ref), longitude_ref = mean(longitude_ref) ) %>%
  ungroup() %>%
  group_by(qid) %>%
  mutate(latitude_ref = lead(latitude_ref), longitude_ref = lead(longitude_ref)) %>%
  ungroup() %>%
  filter(!is.na(latitude_ref) & !is.na(longitude_ref)) %>%
  mutate( distance = gdist(lon.1 = longitude, 
                   lat.1 = latitude, 
                   lon.2 = longitude_ref, 
                   lat.2 = latitude_ref, 
                   units="m")) %>%
  group_by(half) %>%
  mutate(order = select_order(distance) ) %>% 
  ungroup()
  
  # Plot
sequence = seq(min(race_progress$half), max(race_progress$half), 1) #20
race_plot = race_progress %>%
  mutate(qid1 = ifelse(qid == "AFO34112", "1", "0"),
         qid2 = ifelse(qid == "AFQ10923", "1", "0"),
         qid3 = ifelse(qid == "AFQ12544", "1", "0"),
         qid4 = ifelse(qid == "AFQ30790", "1", "0"),
         qid5 = ifelse(qid == "AFQ31151", "1", "0"),
         qid6 = ifelse(qid == "AFQ38198", "1", "0"),
         qid7 = ifelse(qid == "AFQ50464", "1", "0")
         ) %>%
  filter(half %in% sequence)

race_plot %>%
  filter(qid4 == "0") %>%
  ggplot(aes(x = half, y = order, group = qid )) +
  geom_line(alpha = 0.25, size = 1.8) +
  scale_y_reverse(breaks = seq(1:7)) + 
  theme_bw() + 
  geom_line(data = race_plot %>% filter(qid4 == "1"), 
            aes(x = half, y = order, group = qid),
            color = "#bd0026", size = 1.8) +
  geom_point(data = race_plot %>% filter(qid4 == "1"), 
            aes(x = half, y = order, group = qid),
            color = "#bd0026", size = 3) +
  ggtitle("AFQ30790")


race_plot %>%
  ggplot(aes(x = half, y = order, group = qid )) +
  geom_line(alpha = 0.5, size = 1) +
  scale_y_reverse(breaks = seq(1:7)) + 
  theme_bw() +
  facet_grid(qid ~ .)

# ----------------------
### Shape of the peloton
sequence = seq(min(race_progress$half), max(race_progress$half), 50)  #20

# Aaaaaah
race_progress %>% 
  filter(half %in% sequence ) %>%
  group_by(half) %>%
  summarise(lat = max(latitude) - min(latitude),
            long = max(longitude) - min(longitude)) %>%
  ungroup() %>%
  summarise(lat = max(lat), long = max(long))
# Aaaaaaaah

structure = function(i) {
  race_structure = race_progress %>% 
    filter(half == sequence[i]) %>%
    mutate(lat = latitude - min(latitude),
           long = longitude - min(longitude)) 
  
  find_hull <- function(df) df[chull(df$lat, df$long), ]
  hulls <- ddply(race_structure, "half", find_hull)
  
  p = race_structure %>% 
    ggplot(aes(lat,long, group = half)) + 
    scale_x_continuous(limits = c(0,0.004461299)) +
    scale_y_continuous(limits = c(0,0.004766111)) +
    geom_point(size = 0.25) + 
    geom_polygon(data = hulls, alpha = 0.75) + 
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + 
    ggtitle(paste0("Time: ",sequence[i]))
  
  # print(c(min(race_structure$lat), max(race_structure$lat)))
  # print(c(min(race_structure$long), max(race_structure$long)))
  return(p)
}

p1 = structure(1)
p2 = structure(2)
p3 = structure(3)
p4 = structure(4)
p5 = structure(5)
p6 = structure(6)
p7 = structure(7)
p8 = structure(8)
p9 = structure(9)
p10 = structure(10)
p11 = structure(11)
p12 = structure(12)
p13 = structure(13)
p14 = structure(14)
p15 = structure(15)


multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, cols=4)
