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
post100 <- filter(caseSum, totalCases >= 100)
ID100 <- unique(post100$GeoId)
normalizedDate <- data.frame()

for(i in 1:length(ID100)){
  temp100 <- filter(post100, GeoId == ID100[i]) %>% arrange(DateRep)
  temp100[, "timeSince"] <- difftime(temp100$DateRep, temp100$DateRep[1], units = "days")
  normalizedDate <- rbind(normalizedDate, temp100)
}