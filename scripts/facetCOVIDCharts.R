#Plot case and death curves for each country
library(tidyverse)
library(gghighlight)
library(scales)
library(here)
library(ggsci)

COVID <- as_tibble(COVID)

#Limit dataset to 6 countries, mutate to get total case data
limitedCOVID <- filter(COVID, (GeoId %in% c('IT', 'ES', 'KR', 'US', 'FR', 'DE')) & (Month == c(2, 3)))

limitedCOVID <- mutate(limitedCOVID, Count = "daily")

ID <- unique(limitedCOVID$GeoId)
limitedCOVIDSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- limitedCOVID %>% filter(GeoId == ID[i]) %>% arrange(DateRep)
  tempCountry[, "Cases"] <- cumsum(tempCountry$Cases)
  tempCountry[, "Count"] <- "total"
  limitedCOVIDSum <- bind_rows(limitedCOVIDSum, tempCountry)
}

limitedCOVID <- bind_rows(limitedCOVID, limitedCOVIDSum)
background_limitedCOVID <- select(limitedCOVID, -"Countries")

#Plot faceted daily and total case values for 6 countries
ggplot(data = filter(limitedCOVID, Count == "daily"), 
       mapping = aes(x = DateRep, y = Cases, color = Countries, group = GeoId)) + 
  geom_line(data = filter(background_limitedCOVID, Count == "daily"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")

ggplot(data = filter(limitedCOVID, Count == "total"), 
       mapping = aes(x = DateRep, y = Cases, color = Countries, group = GeoId)) + 
  geom_line(data = filter(background_limitedCOVID, Count == "total"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")

#Mutate dataset to get total case data
lateCOVID <- filter(COVID, (Month == c(2, 3)))

lateCOVID <- mutate(lateCOVID, Count = "daily")

ID <- unique(lateCOVID$GeoId)
lateCOVIDSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- lateCOVID %>% filter(GeoId == ID[i]) %>% arrange(DateRep)
  tempCountry[, "Cases"] <- cumsum(tempCountry$Cases)
  tempCountry[, "Count"] <- "total"
  lateCOVIDSum <- bind_rows(lateCOVIDSum, tempCountry)
}

lateCOVID <- bind_rows(lateCOVID, lateCOVIDSum)
upperCOVID <- filter(lateCOVID, Cases >= 500)
upperID <- unique(upperCOVID$GeoId)
lateCOVID <- filter(lateCOVID, GeoId %in% upperID)
background_lateCOVID <- select(lateCOVID, -"Countries")

#Plot faceted daily and total case values

##Daily values
ggplot(data = filter(lateCOVID, Count == "daily"), 
       mapping = aes(x = DateRep, y = Cases, color = Countries, group = GeoId)) + 
  geom_line(data = filter(background_lateCOVID, Count == "daily"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("") 

##Total values
ggplot(data = filter(lateCOVID, Count == "total"), 
       mapping = aes(x = DateRep, y = Cases, color = Countries, group = GeoId)) + 
  geom_line(data = filter(background_lateCOVID, Count == "total"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")

#Normalize start date
normalizedLateCOVID <- data.frame()

for(i in 1:length(upperID)){
  tempLateCOVID <- filter(lateCOVID, GeoId == upperID[i]) %>% arrange(DateRep)
  tempLateCOVID[, "timeSince"] <- difftime(tempLateCOVID$DateRep, tempLateCOVID$DateRep[1], units = "days")
  normalizedLateCOVID <- rbind(normalizedLateCOVID, tempLateCOVID)
}

background_normalizedLateCOVID <- select(normalizedLateCOVID, -"Countries")

##Daily values
ggplot(data = filter(normalizedLateCOVID, Count == "daily"), 
       mapping = aes(x = timeSince, y = Cases, color = Countries, group = GeoId)) + 
  geom_line(data = filter(background_normalizedLateCOVID, Count == "daily"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")

##Total values
ggplot(data = filter(normalizedLateCOVID, Count == "total"), mapping = aes(x = timeSince, y = Cases, color = Countries, group = GeoId)) + 
  geom_line(data = filter(background_normalizedLateCOVID, Count == "total"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  scale_y_log10(breaks = c(100, 1000, 10000, 100000), limits = c(1, 100000)) + 
  annotation_logticks(sides="l") + ylim(1, 100000) + 
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")










