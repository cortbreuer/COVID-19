#Explore COVID-19 cases around the world
library(tidyverse)
library(gghighlight)

#Mapping US only curve
COVID <- as_tibble(COVID)
USA <- COVID %>% filter(Countries == 'United_States_of_America') %>% arrange(DateRep)
ggplot(data = USA, mapping = aes(x = DateRep, y = cumsum(Cases))) + geom_line()

#Generate cumulative case sum datset
ID <- unique(COVID$GeoId)
caseSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- COVID %>% filter(GeoId == ID[i]) %>% arrange(DateRep)
  tempCountry[, "totalCases"] <- cumsum(tempCountry$Cases)
  caseSum <- rbind(caseSum, tempCountry)
}

#Plot cumulative case curves for all countries in datset
ggplot(data = caseSum, mapping = aes(x = DateRep, y = totalCases, group = GeoId, color = GeoId)) + geom_line() + gghighlight(GeoId == c('US', 'ES', 'IT'))

recentCaseSum <- filter(caseSum, Month == 3)
ggplot(data = recentCaseSum, mapping = aes(x = DateRep, y = totalCases, group = GeoId)) + geom_line()

#Normalize curve start dates
post100 <- filter(caseSum, totalCases >= 100)

for(i in 1:length(ID)){
  temp100 <- COVID %>% filter(GeoId == ID[i]) %>% arrange(DateRep)
  temp100[, timeSince] <- 
}
