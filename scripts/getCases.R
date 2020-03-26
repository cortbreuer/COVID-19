#Read European CDC Coronavirus testing data into R
library(readxl)
library(httr)

#create the URL where the dataset is updated daily
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset to a local temporary file
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the dataset sheet into R
COVID <- read_excel(tf)
names(COVID)[names(COVID) == 'Countries and territories'] <- 'Countries'

