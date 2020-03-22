#Explore COVID-19 deaths around the world
library(tidyverse)
library(gghighlight)
library(scales)
library(here)

COVID <- as_tibble(COVID)

#Plot COVID-19 deaths day-by-day
ggplot(data = COVID, mapping = aes(x = DateRep, y = Deaths, group = GeoId, color = GeoId)) + 
  geom_line() + gghighlight(GeoId == 'IT')

#Generate cumulative death data
ID <- unique(COVID$GeoId)
deathSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- COVID %>% filter(GeoId == ID[i]) %>% arrange(DateRep)
  tempCountry[, "totalDeaths"] <- cumsum(tempCountry$Deaths)
  deathSum <- rbind(deathSum, tempCountry)
}

#Normalize dates in dataset to first death
oneDeath <- filter(deathSum, totalDeaths >= 1)
oneID <- unique(oneDeath$GeoId)
normalizedDeathDate <- data.frame()

for(i in 1:length(oneID)){
  tempDeaths <- filter(oneDeath, GeoId == oneID[i]) %>% arrange(DateRep)
  tempDeaths[, "timeSince"] <- difftime(tempDeaths$DateRep, tempDeaths$DateRep[1], units = "days")
  normalizedDeathDate <- rbind(normalizedDeathDate, tempDeaths)
}

#Plot curves for normalized death totals by first death date
ggplot(data = normalizedDeathDate, mapping = aes(x = timeSince, y = totalDeaths, group = GeoId, color = GeoId)) + 
  geom_line() + geom_point() + gghighlight(GeoId == 'IT') + theme_bw() + scale_y_log10(breaks = c(1, 10, 100, 1000, 10000)) + annotation_logticks(sides="l")

#Generate constant growth vectors
day <- seq(0, 30, 1)
third <- data.frame(day = day, growth = ((4/3)^day))

ggplot() + geom_line(data = normalizedDeathDate, mapping = aes(x = timeSince, y = totalDeaths, group = GeoId, color = GeoId)) + 
  geom_point(data = normalizedDeathDate, mapping = aes(x = timeSince, y = totalDeaths, group = GeoId, color = GeoId)) + 
  gghighlight(GeoId == c('IT', 'US')) + scale_y_log10(breaks = c(1, 10, 100, 1000, 10000)) + annotation_logticks(sides="l") + 
  geom_line(data = third, mapping = aes(x = day, y = growth)) + theme_bw()









