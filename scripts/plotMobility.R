#Analyze mobility data from Descartes Lab for correlation of social distancing with COVID-19 cases
library(tidyverse)
library(anytime)
library(ggsci)
library(RColorBrewer)
library(viridis)
library(gghighlight)
library(scales)
library(here)
library(TTR)
library(gganimate)
library(gifski)
library(mapdata)

source("scripts/getLocations.R")

#Plot state-wide aggregate data

ggplot(data = stateMobility, mapping = aes(x = date, y = m50_index, group = admin1, color = admin1)) + 
  geom_line(show.legend = FALSE) + 
  theme_bw() + 
  scale_color_viridis(discrete = TRUE)

ggplot(data = stateMobility, mapping = aes(x = date, y = admin1, fill = m50_index)) + 
  geom_tile() + 
  theme_bw() + 
  scale_fill_viridis()
#scale_fill_gradientn(colors = c("#e7f0fa","#c9e2f6","#95cbee","#0099dc","#4ab04a", "#ffd73e","#eec73a","#e29421","#f05336","#ce472e"))

state <- map_data("state")
usa <- map_data("usa")
stateMobility <- rename(stateMobility, "region" = "admin1")
stateMobility$region <- tolower(stateMobility$region)
stateMob <- left_join(state, stateMobility)

ggplot(data = filter(stateMob, date == "2020-03-01"), mapping = aes(x = long, y = lat, group = group, fill = m50_index)) + 
  geom_polygon(color = "black") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  theme_bw()

first <- filter(stateMob, date == "2020-03-01")
last <- filter(stateMob, date == "2020-03-26")
delta <- (first$m50_index - last$m50_index)/first$m50_index
stateMobDelta[, "delta"] <- delta

ggplot(data = stateMobDelta, mapping = aes(x = long, y = lat, group = group, fill = delta)) + 
  geom_polygon(color = "black") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  theme_bw()

#dggplot() + geom_map(data = , map=state, aes(long, lat, map_id=region), color="#2b2b2b", fill=NA, size=0.5) + 
  #geom_map(data = c(usa, stateMobility), map=usa, aes(long, lat, map_id=region), color="#2b2b2b", fill=NA, size=0.5)

#county <- map_data("county")
#gg <- gg + geom_map(data=county, map=county,
#aes(long, lat, map_id=region),
#color="#2b2b2b", fill=NA, size=0.15)




