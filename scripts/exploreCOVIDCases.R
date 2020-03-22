#Explore COVID-19 cases around the world
library(tidyverse)
library(gghighlight)
library(scales)
library(here)

COVID <- as_tibble(COVID)

#Mapping US-only test curve, day-by-day
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
ggplot(data = caseSum, mapping = aes(x = DateRep, y = totalCases, group = GeoId, color = GeoId)) + 
  geom_line() + gghighlight(GeoId == c('US', 'ES', 'IT'))

recentCaseSum <- filter(caseSum, Month == 3)
ggplot(data = recentCaseSum, mapping = aes(x = DateRep, y = totalCases, group = GeoId)) + geom_line()

#Normalize curve start dates
post100 <- filter(caseSum, totalCases >= 100)
ID100 <- unique(post100$GeoId)
normalizedDate <- data.frame()

for(i in 1:length(ID100)){
  temp100 <- filter(post100, GeoId == ID100[i]) %>% arrange(DateRep)
  temp100[, "timeSince"] <- difftime(temp100$DateRep, temp100$DateRep[1], units = "days")
  normalizedDate <- rbind(normalizedDate, temp100)
}

#Plot COVID-19 case total by country starting at 100 cases
ggplot(data = normalizedDate, mapping = aes(x = timeSince, y = totalCases, group = GeoId, color = GeoId)) + 
  geom_line() + geom_point() + gghighlight(GeoId == c('US', 'ES', 'IT')) + scale_y_log10(breaks = c(100, 1000, 10000, 100000)) + 
  xlim(0, 30) + annotation_logticks(sides="l") + xlim(0, 30) + theme_bw()
ggsave(here("figures", "20200321CasePlot.png"))

#Generate constant growth vectors
day <- seq(0, 20, 1)
third <- data.frame(day = day, growth = (100*(4/3)^day))

ggplot() + geom_line(data = normalizedDate, mapping = aes(x = timeSince, y = totalCases, group = GeoId, color = GeoId)) + 
  geom_point(data = normalizedDate, mapping = aes(x = timeSince, y = totalCases, group = GeoId, color = GeoId)) + 
  gghighlight(GeoId == c('US', 'ES', 'IT')) + scale_y_log10(breaks = c(100, 1000, 10000, 100000)) + xlim(0, 30) + 
  annotation_logticks(sides="l") + xlim(0, 30) + geom_line(data = third, mapping = aes(x = day, y = growth)) + theme_bw()
