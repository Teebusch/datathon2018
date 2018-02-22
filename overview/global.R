
require(chron)
require(tidyverse)
require(ggmap)
# Read data clean
load("data/cycling_clean.RData")
# load("overview/data/cycling_clean.RData")


levels_gears = c("-1 | -1", 
                 "0 | 0", "0 | 1", "0 | 2", "0 | 3", "0 | 4", "0 | 5", "0 | 6",  "0 | 7", "0 | 8", "0 | 9", "0 | 10",
                 "1 | 0", "1 | 1", "1 | 2", "1 | 3", "1 | 4", "1 | 5", "1 | 6",  "1 | 7", "1 | 8", "1 | 9", "1 | 10",  
                 "7 | 7",   "7 | 3")


MaxTable <- function(InVec, mult = FALSE) {
  if (!is.factor(InVec)) InVec <- factor(InVec)
  A <- tabulate(InVec)
  if (isTRUE(mult)) {
    levels(InVec)[A == max(A)]
  } 
  else levels(InVec)[which.max(A)]
}



df2 = df %>% select(qid, ts, latitude, longitude, altitude,
                    speed, heading, power, cadence, heartrate, fd_gear,
                    rd_gear)

df3 = unique(df2) %>% 
  mutate(time = ts - min(df$ts), 
         min = round(time/60),
         speed = as.numeric(speed),
         heartrate = as.numeric(heartrate),
         altitude = as.numeric(altitude),
         power = as.numeric(power),
         cadence = as.numeric(cadence),
         gear = factor( paste(fd_gear, rd_gear, sep = " | "), levels = levels_gears)
         )


df_sum = df3 %>%
  group_by(min, qid) %>%
  summarize(latitude = mean(latitude), 
            longitude = mean(longitude),
            speed = mean(speed),
            heartrate = mean(heartrate),
            altitude = mean(altitude),
            power = mean(power),
            cadence = mean(cadence),
            gear = factor( MaxTable(gear), levels = levels_gears) ) %>%
  ungroup () %>%
  rename(time = min)