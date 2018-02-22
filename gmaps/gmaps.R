#Adding Google Maps

center = paste(min(df$latitude)+(max(df$latitude)-min(df$latitude))/2,
               min(df$longitude)+(max(df$longitude)-min(df$longitude))/2, sep=" ")
map <- get_map(location = center, zoom = 10, maptype = "terrain", source = "google")

ggmap(map) + 
  geom_path(data = df, aes(x = longitude, y = latitude, color = qid), 
            show.legend = FALSE) +
  facet_wrap(~qid)
