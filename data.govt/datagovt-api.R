library(tidyverse)
library(sf)
library(httr)
library(rvest)
library(jsonlite)
library(snakecase)

url <- 'https://catalogue.data.govt.nz/api/3/action/package_search'
result <- GET(url)
tmp <- fromJSON(content(result, as='text'))

## API request loop
rows <- 1000
stop <- 0
i <- 1
start <- i
dat <- list()
while(stop < 1) {
  url_page <- paste0(url,'?rows=',rows,'&start=',start)
  tmp <- GET(url_page)
  dat[[i]] <- tmp
  stop <- if(fromJSON(content(tmp, as='text'))$result$count <= start + rows){1}else{0}
  start <- start+rows
  i <- i+1
}
rm(i, stop, start, rows, url_page)

## melt paginated data together
df <- bind_rows(lapply(dat, function(x) { fromJSON(content(x, as='text'))$result$results %>% as.data.frame() }))

saveRDS(df, 'datasets/catalogue.rds')

catalogue <- readRDS('datasets/catalogue.rds')

####################################################################################################

ids <- sample(catalogue$id)
p_list <- list()
for(i in 1:100) {
  url <- paste0('https://catalogue.data.govt.nz/api/3/action/package_show?id=', ids[i])
  result <- GET(url)
  tmp <- fromJSON(content(result, as='text'))
  p_list[[i]] <- tmp$result
}

p_list[[1]]$resources %>% select(name, url)
####################################################################################################

## Motor Vehicle Register 'https://nztaopendata.blob.core.windows.net/motorvehicleregister/Fleet-data-all-vehicle-years.zip'
## Waka Kotahi - Motor Vehicle register

url <- 'https://nztaopendata.blob.core.windows.net/motorvehicleregister/Fleet-data-all-vehicle-years.zip'
temp <- tempfile()
download.file(url,temp)
data <- read.table(unz(temp, "*.csv"))
unlink(temp)

url <- 'https://nztaopendata.blob.core.windows.net/motorvehicleregister/VehicleYear-2021.csv'
df <- read.csv2(url,header = T, sep = ',')
names(df) <- to_any_case(names(df), 'snake')
df <- df %>% as_tibble()

####################################################################################################

url <- 'https://catalogue.data.govt.nz/api/3/action/group_list'
result <- GET(url)
tmp <- fromJSON(content(result, as='text'))
result
tmp

####################################################################################################

