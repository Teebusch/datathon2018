library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(forcats)


df <- read_rds("cycling.rds")

df <- df %>% 
  group_by(id, day) %>%
  mutate(stretch = 
           scale(dist) > 100 | 
           (time - lag(time, default = first(time))) > 3,
         stretch = cumsum(stretch)
         )

#dfo <- df
  
# df <- df %>% 
#   filter(dayid == 3, id == "10")


r_curve_type <- function(x, thresh = .50) {
  props <- table(x) / length(x)
  mode_p <- props[which.max(props)]
  if(mode_p > thresh) {
    names(mode_p)
  } else {
    "none"
  }
}

# tests 
r_curve_type(rep(c("left", "right", NA), times = c(1,4,5))) == "none"
r_curve_type(rep(c("left", "right", NA), times = c(5,1,1))) == "left"
r_curve_type(rep(c("left", "right", NA), times = c(1,5,1))) == "right"

df <- df %>%
  group_by(id, day, stretch) %>%
  mutate(
    r_heading = rollapply(d_heading, 5, sum, align = "center", fill = NA),
    curve = case_when(r_heading < -0 ~  "left", 
                      r_heading > +0 ~ "right",
                      TRUE ~ "none"),
    curve = rollapply(curve, 5, function(x) r_curve_type(x, 0), align = "center", 
                      fill = "none"),
    # curve = rollapply(curve, 10, function(x) r_curve_type(x, thresh = 0),
    #                   align = "center", fill = "none"),
    idcurve = (curve != lag(curve)) %>% 
      coalesce(FALSE),
    idcurve = paste(id, dayid, stretch, cumsum(idcurve), sep = ".")
  )

(p <- df %>%
    ggplot(aes(latitude, longitude, group = idcurve, 
               color = curve)) +
    geom_point(show.legend = F, size = .2) +
    scale_color_manual(values = c("cornflowerblue", "grey80", "tomato")) +
    theme(panel.grid = element_blank()) +
    facet_wrap(~ id) +
    coord_cartesian(xlim = c(37.625,37.7), ylim = c(23.13, 23.16))
)

df_curves <- df %>%
  group_by(day, id, stretch, idcurve) %>%
  summarize(
    duration = diff(range(ts)),
    dist = sum(dist),
    d_heading = sum(d_heading),
    turn_meter = abs(d_heading)/dist,
    turn_second = abs(d_heading)/duration,
    avg_speed = mean(speed),
    red_speed = min(speed) / max(speed),
    change_speed = first(speed) / last(speed),
    gear_shifts = length(unique(fd_gear)) + length(unique(rd_gear))
  )

df_curves <- df_curves %>%
  filter(duration >= 15,
         abs(d_heading) > 15,
         between(turn_meter, 0.1, 3),
         between(turn_second, .5, 8),
         between(dist, 5, 500))

df2 <- df %>%
  filter(
    idcurve %in% df_curves$idcurve,
    curve != "none"
  )

(p <- df2 %>%
    filter(id == "10", dayid == 3) %>%
    ggplot(aes(latitude, longitude, group = idcurve, color = curve)) +
    geom_point(data = filter(df, id == "10", dayid == 3), color = "grey90",
              aes(latitude, longitude), 
              size = .5) +
    geom_line(show.legend = F, size = 1) +
    scale_color_manual(values = c("cornflowerblue", "tomato")) +
    theme(panel.grid = element_blank(), strip.text = element_blank()) +
    facet_wrap(~ id) +
    coord_cartesian(xlim = c(37.625,37.7), ylim = c(23.13, 23.16))
)
ggplotly(p)


# plot curves

df_curves %>% 
  ggplot(aes(dist, avg_speed)) + 
  geom_point()

df_curves %>% 
  ggplot(aes(duration, red_speed)) + 
  geom_point()

df_curves %>% 
  ggplot(aes(turn_second, change_speed)) + 
  geom_point() +
  coord_cartesian(ylim = c(0, 10))

df_curves %>% 
  ggplot(aes(turn_meter, avg_speed)) + 
  geom_point(color = "black", alpha = .6, size = 4) + 
  theme_classic() +
  labs(x = "Turn Sharpness (deg/m)", y = "Speed (m/s)")

ggsave("turncor.png", width = 10, height = 10)

df_curves %>%
  filter(dist > 30) %>%
  ggplot(aes(turn_meter, red_speed)) + 
  geom_point()

df_curves %>% 
  ggplot(aes(turn_second, avg_speed)) + 
  geom_point()


df_curves %>%
  ggplot(aes(dist, red_speed)) + 
  geom_point()

df_curves %>%
  ggplot(aes(turn_meter, gear_shifts)) + 
  geom_point() +
  facet_wrap(~ id)


minmax <- function(x) {
  (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))  
}


df2 <- df %>%
  filter(
    idcurve %in% df_curves$idcurve,
    curve != "none"
  )

df2 <- df2 %>%
  group_by(idcurve) %>%
  mutate(percent_done = (row_number()-1)/n(),
         percent_bin = percent_done %/% 0.1)

df3 <- df2 %>% 
  group_by(id, dayid, idcurve, percent_bin) %>%
  summarize(
    dist = sum(dist),
    power = mean(power),
    cadence = mean(cadence),
    duration = diff(range(ts)),
    red_speed = min(speed) / max(speed),
    change_speed = first(speed) / last(speed),
    change_altitude = max(altitude) - min(altitude),
    speed = mean(speed, na.rm = T),
    d_heading = sum(d_heading),
    turn_meter = abs(d_heading)/dist,
    turn_second = abs(d_heading)/duration,
    gear_shifts = length(unique(fd_gear)) + length(unique(rd_gear))
  ) %>%
  group_by(id, dayid, idcurve) %>%
  mutate(
    power_percent = minmax(power)*100,
    speed_percent = minmax(speed)*100,
    speed_percent2 = minmax((speed)/first(speed))*100,
    cadence_percent = minmax(cadence)*100,
    red_speed = minmax(red_speed),
    change_speed = minmax(change_speed),
    curve_length = sum(dist),
    curve_bend = abs(sum(d_heading))
  ) %>% 
  group_by(id, dayid) %>%
  filter(between(curve_length, 0 , 500)) %>%
  mutate(
    curve_length_rank = dense_rank(curve_length),
    curve_bend_rank = dense_rank(curve_bend)
  )
  

df3 %>%
  mutate(iid = paste(id, dayid)) %>%
  filter(
    !(iid  %in% c("5 2", "6 1"))
  ) %>%
  ggplot(aes(x = as.factor(percent_bin), y = curve_bend_rank, 
             group = idcurve)) +
  geom_raster(aes(fill = speed_percent)) +
  scale_fill_distiller(type = "seq", palette = 13, name = "Speed (%)") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0.01, "cm"),
        panel.background = element_blank()
        #legend.position = "none"
        ) +
  facet_wrap(~ id + dayid, scales = "free") +
  labs(x = "Position in Turn (%)", y = "Sharpness of Turn", 
       title = "How to Ride a Turn?")


ggsave("curvespeed.png", height = 10, width = 10)





df3 %>%
  #filter(dayid == 3) %>%
  mutate(iid = paste(id, dayid)) %>%
  ggplot(aes(x = as.factor(percent_bin), y = curve_bend_rank, 
             group = idcurve)) +
  geom_raster(aes(fill = gear_shifts)) +
  scale_fill_distiller(type = "seq", palette = 3) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"
  ) +
  facet_wrap(~ iid, scales = "free") +
  labs(x = "Position in Curve (%)", y = "Sharpness of Curve", 
       title = "How to ride a curve?")




df %>%
  filter(dayid == 1) %>% View()

qmplot(longitude, latitude, data = df)

qmplot(longitude, latitude, data = df %>% filter(day == 1))

df_curves %>% filter(idcurve == "10.3.104.1")
