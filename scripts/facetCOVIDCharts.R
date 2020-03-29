#Plot case and death curves for each country
library(tidyverse)
library(gghighlight)
library(scales)
library(here)
library(ggsci)

source("scripts/getCases.R")
COVID <- as_tibble(COVID)

#Limit dataset to 6 countries, mutate to get total case data
limitedCOVID <- filter(COVID, geoId %in% c('IT', 'ES', 'KR', 'US', 'FR', 'DE'))

limitedCOVID <- mutate(limitedCOVID, Count = "daily")

ID <- unique(limitedCOVID$geoId)
limitedCOVIDSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- limitedCOVID %>% filter(geoId == ID[i]) %>% arrange(dateRep)
  tempCountry[, "cases"] <- cumsum(tempCountry$cases)
  tempCountry[, "Count"] <- "total"
  limitedCOVIDSum <- bind_rows(limitedCOVIDSum, tempCountry)
}

limitedCOVID <- bind_rows(limitedCOVID, limitedCOVIDSum)
background_limitedCOVID <- select(limitedCOVID, -"Countries")

#Plot faceted daily and total case values for 6 countries
limitedCOVIDDaily <- ggplot(data = filter(limitedCOVID, Count == "daily"), mapping = aes(x = dateRep, y = cases, color = Countries, group = geoId)) + 
  geom_line(data = filter(background_limitedCOVID, Count == "daily"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")

limitedCOVIDTotal <- ggplot(data = filter(limitedCOVID, Count == "total"), mapping = aes(x = dateRep, y = cases, color = Countries, group = geoId)) + 
  geom_line(data = filter(background_limitedCOVID, Count == "total"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")

ggsave(plot = limitedCOVIDDaily, "figures/2020-03-29_Limited-Facet_Daily-Cases.png", width = 10, height = 6)
ggsave(plot = limitedCOVIDTotal, "figures/2020-03-29_Limited-Facet_Case-Total.png", width = 10, height = 6)

#Mutate dataset to get total case data
lateCOVID <- COVID
  
#filter(COVID, (Month == c(2, 3)))

lateCOVID <- mutate(lateCOVID, Count = "daily")

ID <- unique(lateCOVID$geoId)
lateCOVIDSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- lateCOVID %>% filter(geoId == ID[i]) %>% arrange(dateRep)
  tempCountry[, "cases"] <- cumsum(tempCountry$cases)
  tempCountry[, "Count"] <- "total"
  lateCOVIDSum <- bind_rows(lateCOVIDSum, tempCountry)
}

lateCOVID <- bind_rows(lateCOVID, lateCOVIDSum)
upperCOVID <- filter(lateCOVID, cases >= 1400)
upperID <- unique(upperCOVID$geoId)
lateCOVID <- filter(lateCOVID, geoId %in% upperID)
background_lateCOVID <- select(lateCOVID, -"Countries")

#Plot faceted daily and total case values

##Daily values
ggplot(data = filter(lateCOVID, Count == "daily"), 
       mapping = aes(x = dateRep, y = cases, color = Countries, group = geoId)) + 
  geom_line(data = filter(background_lateCOVID, Count == "daily"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("") 

##Total values
ggplot(data = filter(lateCOVID, Count == "total"), mapping = aes(x = dateRep, y = cases, color = Countries, group = geoId)) + 
  geom_line(data = filter(background_lateCOVID, Count == "total"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("")

#Normalize start date
normalizedLateCOVID <- data.frame()

for(i in 1:length(upperID)){
  tempLateCOVID <- filter(lateCOVID, geoId == upperID[i]) %>% arrange(dateRep)
  tempLateCOVID[, "timeSince"] <- difftime(tempLateCOVID$dateRep, tempLateCOVID$dateRep[1], units = "days")
  normalizedLateCOVID <- rbind(normalizedLateCOVID, tempLateCOVID)
}

background_normalizedLateCOVID <- select(normalizedLateCOVID, -"Countries")

##Daily values
facetNormalizedCOVIDDaily <- ggplot(data = filter(normalizedLateCOVID, Count == "daily"), mapping = aes(x = timeSince, y = cases, color = Countries, group = geoId)) + 
  geom_line(data = filter(background_normalizedLateCOVID, Count == "daily"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlab("Days Since First Case")

ggsave(plot = facetNormalizedCOVIDDaily, "figures/2020-03-29_Facet_Daily-Cases.png", width = 10, height = 7)

#Select for common start case number
lateCOVID100 <- filter(normalizedLateCOVID, cases >= 100)
normalizedLateCOVID100 <- data.frame()

for(i in 1:length(upperID)){
  tempLateCOVID100 <- filter(lateCOVID100, geoId == upperID[i]) %>% arrange(dateRep)
  tempLateCOVID100[, "timeSince"] <- difftime(tempLateCOVID100$dateRep, tempLateCOVID100$dateRep[1], units = "days")
  normalizedLateCOVID100 <- rbind(normalizedLateCOVID100, tempLateCOVID100)
}

background_normalizedLateCOVID100 <- select(normalizedLateCOVID100, -"Countries")

##Total values
facetNormalizedCOVIDTotal_lin <- ggplot(data = filter(normalizedLateCOVID100, Count == "total"), mapping = aes(x = timeSince, y = cases, color = Countries, group = geoId)) + 
  geom_line(data = filter(background_normalizedLateCOVID100, Count == "total"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  facet_wrap(. ~ Countries) + 
  theme_bw() + 
  xlim(0, 30) +
  xlab("")

facetNormalizedCOVIDTotal_exp <- ggplot(data = filter(normalizedLateCOVID100, Count == "total"), mapping = aes(x = timeSince, y = cases, color = Countries, group = geoId)) + 
  geom_line(data = filter(background_normalizedLateCOVID100, Count == "total"), color = "grey", alpha = .8) + 
  geom_line(show.legend = FALSE) +
  scale_y_log10(breaks = c(100, 1000, 10000, 100000), limits = c(100, 100000)) + 
  annotation_logticks(sides="l") + 
  facet_wrap(. ~ Countries) + 
  xlim(0, 30) +
  theme_bw() + 
  xlab("Days Since 100th Case")

ggsave(plot = facetNormalizedCOVIDTotal_lin, "figures/2020-03-29_Facet-Lin_Case-Total.png", width = 10, height = 7)
ggsave(plot = facetNormalizedCOVIDTotal_exp, "figures/2020-03-29_Facet-Exp_Case-Total.png", width = 10, height = 7)










