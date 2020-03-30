#Plot log scale daily cases and total cases for log-curve prediction
library(tidyverse)
library(gghighlight)
library(scales)
library(here)
library(ggsci)
library(TTR)
library(ggsci)
library(gganimate)
library(gifski)

source("scripts/getCases.R")
COVID <- as_tibble(COVID)

#Process data into sum form
COVID <- rename(COVID, "dailyCases" = "cases", "dailyDeaths" = "deaths")

ID <- unique(COVID$geoId)
COVIDSum <- data.frame()

for(i in 1:length(ID)){
  tempCountry <- COVID %>% filter(geoId == ID[i]) %>% arrange(dateRep)
  tempCountry[, "totalCases"] <- cumsum(tempCountry$dailyCases)
  tempCountry[, "totalDeaths"] <- cumsum(tempCountry$dailyDeaths)
  COVIDSum <- bind_rows(COVIDSum, tempCountry)
}

COVID <- COVIDSum

COVIDTop <- filter(COVID, totalCases >= 2500)
IDTop <- unique(COVIDTop$geoId)
COVID <- filter(COVID, geoId %in% IDTop)
COVIDAvg <- data.frame()

for (i in 1:length(ID)){
  tempCountry <- COVID %>% filter(geoId == IDTop[i]) %>% arrange(dateRep)
  tempCountry[, "dailyCases"] <- frollmean(tempCountry$dailyCases, 5)
  tempCountry[, "totalCases"] <- frollmean(tempCountry$totalCases, 5)
  tempCountry[, "dailyDeaths"] <- frollmean(tempCountry$dailyDeaths, 5)
  tempCountry[, "totalDeaths"] <- frollmean(tempCountry$totalDeaths, 5)
  COVIDAvg <- bind_rows(COVIDAvg, tempCountry)
}

COVID <- COVIDAvg
COVID <- filter(COVID, totalCases >= 10, totalDeaths >= 10)

#Plot log-log values in animated form for cases and deaths

loglogCasePlot <- ggplot(data = COVID) + 
  geom_point(mapping = aes(x = totalCases, y = dailyCases, color = geoId)) +
  geom_line(mapping = aes(x = totalCases, y = dailyCases, color = geoId)) + 
  scale_y_log10(limits = c(10, 20000)) + 
  scale_x_log10(limits = c(10, 150000)) + 
  annotation_logticks(sides="lb") + 
  transition_reveal(dateRep) + 
  theme_bw()

animate(loglogCasePlot, renderer = gifski_renderer(loop = F), end_pause = 60, width = 800, height = 600)
anim_save("../figures/loglogCasePlot.gif")

loglogDeathPlot <- ggplot(data = COVID) + 
  geom_point(mapping = aes(x = totalDeaths, y = dailyDeaths, color = geoId)) +
  geom_line(mapping = aes(x = totalDeaths, y = dailyDeaths, color = geoId)) + 
  scale_y_log10(limits = c(10, 1000)) + 
  scale_x_log10(limits = c(10, 10000)) + 
  annotation_logticks(sides="lb") + 
  transition_reveal(dateRep) + 
  theme_bw()

animate(loglogDeathPlot, renderer = gifski_renderer(loop = F), end_pause = 60, width = 800, height = 600)
anim_save("../figures/loglogDeathPlot.gif", width = 8, height = 8)



