library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(ggmap)


df <- read_rds("cycling.rds")

dft <- df %>% 
  filter(dayid == 3, id == "10")
  

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

df <- dft %>%
  group_by(id, day) %>%
  mutate(
    r_heading = rollapply(d_heading, 15, mean, align = "center", fill = NA),
    curve = case_when(r_heading < -0.3 ~  "left", 
                      r_heading > +0.3 ~ "right",
                      TRUE ~ "none"),
    curve = rollapply(curve, 10, function(x) r_curve_type(x), align = "left", 
                      fill = "none"),
    curve = rollapply(curve, 10, function(x) r_curve_type(x, thresh = 0), 
                      align = "left", fill = "none"),
    idcurve = (curve != lag(curve)) %>% 
      coalesce(FALSE),
    idcurve = cumsum(idcurve)
  )

df_curves <- df %>%
  group_by(day, id, idcurve) %>%
  summarize(
    duration = diff(range(ts)),
    dist = sum(dist),
    d_heading = sum(d_heading)
  )

df_curves <- df_curves %>%
  filter(duration > 5, 
         dist > 15,
         abs(d_heading) > 15)

df_curves

# curve vs. speed

df2 <- df %>%
  filter(
    idcurve %in% df_curves$idcurve,
    curve != "none"
  )


(p <- df2 %>%
  filter(id == "10", dayid == 3) %>%
  ggplot(aes(latitude, longitude, group = idcurve, color = curve)) +
  geom_path(data = filter(df, id == "10", dayid == 3), 
            aes(latitude, longitude), color = "grey85") +
  geom_line(show.legend = F) +
  #geom_point(show.legend = F, size = .1) +
  scale_color_manual(values = c("cornflowerblue", "tomato")) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ id) +
  coord_cartesian(xlim = c(37.625,37.7), ylim = c(23.13, 23.16))
)

ggplotly(p)


df %>%
  filter(dayid == 1) %>% View()

qmplot(longitude, latitude, data = df)

qmplot(longitude, latitude, data = df %>% filter(day == 1))
