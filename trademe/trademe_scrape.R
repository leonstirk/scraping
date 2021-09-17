library(tidyverse)
library(httr)
library(stringr)
library(rlist)
library(jsonlite)
library(sf)
library(tmap)
library(sjPlot)
library(tidytext)
## library(quanteda)
library(snakecase)

today <- to_any_case(as.character(Sys.Date()), 'snake')

## load keys
source('trademe.env')

header <- paste('OAuth oauth_consumer_key=', oauth_consumer_key,
      ',oauth_token=', oauth_token,
      ',oauth_signature_method=', oauth_signature_method,
      ',oauth_signature=', oauth_signature,
      sep = '')

## All NZ
## url <- "https://api.trademe.co.nz/v1/Search/Property/Residential.json"
## Region 15 = Wellington
url <- "https://api.trademe.co.nz/v1/Search/Property/Residential.json?region=15"

## Scraping loop
rows <- 500
stop <- 0
i <- 1
catalogue_list <- list()
while(stop < 1) {
  if(str_detect(url,'\\?')) {
    url_page <- paste0(url,'&rows=',rows,'&page=',i)
  } else {
    url_page <- paste0(url,'?rows=',rows,'&page=',i)
  }
  tmp <- GET(url_page, add_headers(Authorization = header))
  catalogue_list[[i]] <- tmp
  stop <- if(ceiling(fromJSON(content(tmp, as='text'))$TotalCount/rows) == i){1}else{0}
  i <- i+1
}
rm(i, stop, rows, url_page)

## melt paginated data together
catalogue_df <- bind_rows(lapply(catalogue_list, function(x) { fromJSON(content(x, as='text'))$List %>% as.data.frame() }))
names(catalogue_df) <- to_any_case(names(catalogue_df), 'snake')

## Clean up variables
catalogue_df$listing_id <- as.character(catalogue_df$listing_id)

catalogue_df$price_integer <- catalogue_df$price_display %>% str_extract_all('\\d') %>%
  lapply(., function(x) { paste(x, collapse = '') }) %>% unlist %>% as.integer()

catalogue_df$price_category <- catalogue_df$price_display %>% str_extract_all('[^\\d:,$]') %>%
  lapply(., function(x) { paste(x, collapse = '') }) %>% unlist %>% str_remove(' $') %>% as.factor

catalogue_df$category_path <- catalogue_df$category_path %>% str_remove_all('/Trade-Me-Property/') %>% as.factor()

convDate <- function(x) { as.Date((x %>% str_remove_all('[/Date()]') %>% as.numeric())/1000/60/60/24, origin = "1970-01-01") }
catalogue_df$start_date <- convDate(catalogue_df$start_date)
catalogue_df$end_date   <- convDate(catalogue_df$end_date)
catalogue_df$as_at <- convDate(catalogue_df$as_at)
rm(convDate)

f <- c('property_type', 'district', 'region', 'suburb')
catalogue_df[,f]  <- lapply(catalogue_df[,f], as.factor)

catalogue_df$lon <- catalogue_df$geographic_location$Longitude
catalogue_df$lat <- catalogue_df$geographic_location$Latitude

names(catalogue_df)[which(names(catalogue_df) == 'area')] <- 'floor_area'
catalogue_df$access_date <- Sys.Date()

## Save catalogue data frame
## filename <- paste('datasets/catalogue_', today, '.rds', sep = '')
## saveRDS(catalogue_df, filename)

##########################################################################################

## Get individual listing info
## https://api.trademe.co.nz/v1/Listings/{listingId}.{file_format}

## sample listing_ids
## ids <- sample(catalogue_df$listing_id, 1000)
## or get all ids
ids <- catalogue_df$listing_id
## Scraping loop
listing_list <- list()
for(i in 1:length(ids)) {
  url_listing <- paste0('https://api.trademe.co.nz/v1/Listings/',ids[i],'.json')
  tmp <- GET(url_listing, add_headers(Authorization = header))
  listing_list[[i]] <- tmp

  sl <- runif(1,0.5,2)
  Sys.sleep(sl)
}

listing_df <- bind_rows(lapply(listing_list, function(x) {
  a <- fromJSON(content(x, as='text'))
  if(!any(!is.na(a$Error))){
    at <- a$Attributes
    ot_n <- c('ListingId', 'BidderAndWatchers', 'ViewCount', 'Title', 'Body')
    ot <- a[ot_n]
    names(ot) <- ot_n
    ot <- lapply(ot, function(x) { if(is.null(x)) { NA } else { x } }) %>% unlist
    b <- c(ot, at[,'Value'])
    names(b) <- c(ot_n, at[,'Name'])
    return(b)
  }
}))
## All var names to snake case
names(listing_df) <- to_any_case(names(listing_df), 'snake')

listing_df$bedrooms <- sapply(listing_df$bedrooms, function(x) {
  if(!is.na(x) & str_detect(x, 'bedroom')) { as.numeric(str_remove_all(x, '[:alpha:]')) }
  else if (is.na(x)) { NA }
})

listing_df$bathrooms <- sapply(listing_df$bathrooms, function(x) {
  if(!is.na(x) & str_detect(x, 'bathroom')) { as.numeric(str_remove_all(x, '[:alpha:]')) }
  else if (is.na(x)) { NA }
})

listing_df$floor_area <- sapply(listing_df$floor_area, function(x) {
  if(!is.na(x) & str_detect(x, 'm²')){ as.numeric(str_remove_all(x, 'm²')) }
  else if (is.na(x)) { NA }
})

listing_df$land_area <- sapply(listing_df$land_area, function(x) {
  if(!is.na(x) & str_detect(x, 'm²')){ as.numeric(str_remove_all(x, 'm²'))/10000 }
  else if(!is.na(x) & str_detect(x, 'hectare')) { as.numeric(str_remove_all(x, '[:alpha:]')) }
  else if (is.na(x)) { NA }
})

n <- c('view_count', 'bidder_and_watchers')
listing_df[,n]  <- lapply(listing_df[,n], as.numeric)

f <- c('property_type', 'district', 'region')
listing_df[,f]  <- lapply(listing_df[,f], as.factor)

rm(n,f)

names(listing_df)[which(names(listing_df) == 'price')] <- 'price_display'
names(listing_df)[which(names(listing_df) == 'rateable_value_rv')] <- 'rateable_value'

## Save tibble of individual listing queries
## filename <- paste('datasets/listing_', today, '.rds', sep = '')
## saveRDS(listing_df, filename)

####################################################################

## Merge individual columns with catalogue df
drop_names <- names(listing_df)[which(!names(listing_df) %in% names(catalogue_df))]
merged_df <- left_join(listing_df[,c('listing_id', drop_names)], catalogue_df, by = 'listing_id')
rm(drop_names)

merged_df$ln_price <- log(merged_df$price_integer)
merged_df$ln_rv <- log(merged_df$rateable_value)

## filename <- paste('datasets/merged_', today, '.rds', sep = '')
## saveRDS(merged_df, filename)

####################################################################

master_df <- readRDS('datasets/master_df.rds')
filename <- paste('datasets/master_df_', today, '.rds', sep = '')
saveRDS(master_df, filename)
old_df <- master_df[which(!master_df$listing_id %in% merged_df$listing_id),]
## new_df <- merged_df[which(!merged_df$listing_id %in% master_df$listing_id),]
master_df <- bind_rows(old_df, merged_df)
saveRDS(master_df, 'datasets/master_df.rds')

####################################################################

## rm(list = ls())
## dat <- readRDS('datasets/master_df.rds')
## dat_sf <- dat %>% filter(access_date == Sys.Date()) %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
## tmap_mode('view')
## tm_shape(dat_sf) + tm_dots(col = 'price_integer')
