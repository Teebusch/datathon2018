library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(ggmap)


df <- read_rds("cycling.rds")

dft <- df %>% 
  filter(dayid == 3)
  

r_curve_type <- function(x, thresh = .60) {
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


dft <- dft %>%
  filter(id == 10) %>%
  group_by(id, day) %>%
  filter(row_number() > 1000) %>%
  mutate(r_heading = rollapply(d_heading, 10, mean, align = "center", fill = NA),
         curve_type = case_when(
           r_heading < 0 ~  "left", 
           r_heading > 0 ~ "right",
           TRUE ~ "none"),
         curve = rollapply(curve_type, 10, r_curve_type, align = "center", fill = "none")
         )

(p <- dft %>%
  filter(id == 10) %>%
  ggplot(aes(latitude, longitude, color = curve)) +
  geom_point(show.legend = F, size = .1) +
  scale_color_manual(values = c("cornflowerblue", "grey80", "tomato")) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ id) +
  coord_cartesian(xlim = c(37.625,37.7), ylim = c(23.13, 23.16))
)

ggplotly(p)


df %>%
  filter(dayid == 1) %>% View()

qmplot(longitude, latitude, data = df)

qmplot(longitude, latitude, data = df %>% filter(day == 1))
