#Animate coronavirus case growth by continual plotting
library(tidyverse)
library(gghighlight)
library(scales)
library(here)
library(ggsci)
library(gganimate)
library(gifski)

source(here("scripts", "getCases.R"))
COVID <- as_tibble(COVID)

#Generate cumulative case and death sum datset
ID <- unique(COVID$GeoId)
caseSum <- data.frame()
deathSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- COVID %>% filter(GeoId == ID[i]) %>% arrange(DateRep)
  tempCountry[, "totalCases"] <- cumsum(tempCountry$Cases)
  caseSum <- rbind(caseSum, tempCountry)
}

for(i in 1:length(ID)){
  tempCountry <- caseSum %>% filter(GeoId == ID[i]) %>% arrange(DateRep)
  tempCountry[, "totalDeaths"] <- cumsum(tempCountry$Deaths)
  deathSum <- rbind(deathSum, tempCountry)
}

COVID <- deathSum
logCOVID <- filter(COVID, (Month == 3) & (totalDeaths >= 1) & (totalCases >= 1))

#
ggplot(data = logCOVID, mapping = aes(x = Cases, y = totalCases, color = totalDeaths, group = Countries)) + 
  geom_point(size = 6, alpha = .6) + transition_time(DateRep)

animate(plot)

