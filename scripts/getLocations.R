#Read location mobility data into R from Descartes Labs
library(RCurl)
library(tidyverse)
library(anytime)
library(ggsci)
library(RColorBrewer)
library(viridis)

url <- getURL("https://raw.githubusercontent.com/descarteslabs/DL-COVID-19/master/DL-us-mobility-daterow.csv")
mobility <- read_csv(url)

stateMobility <- filter(mobility, admin_level == 1)
countyMobility <- filter(mobility, admin_level == 2)
