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


####################################################################
## Text mining
## Clean title text

words <- unnest_tokens(merged_df, word, 'title')

words <- words %>%
  anti_join(stop_words, by = "word")

words_count <- words %>%
  group_by(property_type, word) %>%
  count() %>% arrange(-n)

apartment_words <- words_count %>%
  filter(property_type == "Apartment") %>%
  arrange(-n)

bigrams <- unnest_tokens(merged_df, bigram, 'body', token = "ngrams", n = 2)
bigrams <- bigrams %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
         str_detect(second, "[a-z]"))

bigrams_count <- bigrams %>%
  group_by(bigram) %>%
  count() %>%
  arrange(-n)
bigrams_count

getBigramsByGroup <- function(dat, input_col, grouping_col, group_name) {
  bigrams <- unnest_tokens(dat, bigram, input_col, token = "ngrams", n = 2)
  bigrams <- bigrams %>%
    separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
    anti_join(stop_words, by = c("first" = "word")) %>%
    anti_join(stop_words, by = c("second" = "word")) %>%
    filter(str_detect(first, "[a-z]") &
             str_detect(second, "[a-z]"))

  bigrams_count <- bigrams %>%
    group_by(!!sym(grouping_col), bigram) %>%
    count()

  group_bigrams <- bigrams_count %>%
    filter(!!sym(grouping_col) == group_name) %>%
    arrange(-n)

  return(group_bigrams)
}
getBigramsByGroup(listing_df, 'body', 'property_type', 'Apartment')


trigrams <- unnest_tokens(merged_df, trigram, 'body', token = "ngrams", n = 3)
trigrams <- trigrams %>%
  separate(trigram, into = c("first","second","third"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  anti_join(stop_words, by = c("third" = "word")) %>%
  filter(str_detect(first,  "[a-z]") &
         str_detect(second, "[a-z]") &
         str_detect(third,  "[a-z]"))

trigrams_count <- trigrams %>%
  group_by(trigram) %>%
  count() %>%
  arrange(-n)
trigrams_count

## which listings contain any of the top 5 most popular bigrams
cliches <- listing_df[which(listing_df$ListingId %in% bigrams$ListingId[which(bigrams$bigram %in% bigrams_count$bigram[1:5])]),]

## Compare distribution of property types of full sample vs cliche sample
par(mfrow = c(2))
plot(df_listing$property_type %>% as.factor())
plot(cliches$property_type %>% as.factor())

##########################################################################################

## random forest estimation
library(randomForest)

dat <- merged_df %>%
  filter(price_integer < 2000000 & price_integer > 250000) %>%
  select(ln_price, price_category, region, bedrooms, bathrooms, land_area, floor_area, property_type) %>%
  na.omit()

ttv <- gespag::TTV(dat)
fit1 <- randomForest(ln_price ~ ., data = ttv[['train']])
plot(fit1)

oob.err <- double(7)
test.err <- double(7)
val.err <- double(7)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:7)
{
  rf <- randomForest(ln_price ~ . , data = ttv[['train']], mtry=mtry, ntree=400)
  oob.err[mtry] <- rf$mse[400] #Error of all trees fitted

  pred <- predict(rf, ttv[['test']]) #Predictions on Test Set for each Tree
  test.err[mtry] <- with(ttv[['test']], mean((ln_price - pred)^2)) ## Mean squared test error

  pred <- predict(rf, ttv[['validate']]) ## Predictions on the validation set for each tree
  val.err[mtry] <- with(ttv[['validate']], mean((ln_price - pred)^2)) ## Mean squared validation error

  cat(mtry," ") #printing the output to the console
}

matplot(1:mtry , cbind(oob.err,test.err, val.err), pch=19 , col=c("red","blue","green"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out-of-bag error","Test error", "Validation error"),pch=19, col=c("red","blue","green"))

fit2 <- randomForest(ln_price ~ ., data = ttv[['train']], mtry = 4, ntree = 400, importance = T)

sqrt(mean((predict(fit2, ttv[['test']]) - ttv[['test']]$ln_price)^2))
sqrt(mean((predict(fit2, ttv[['validate']]) - ttv[['validate']]$ln_price)^2))

##########################################################################################

dat$ln_price %>% hist
df$price_category %>% plot

## Transform to sf
df_sf <- st_as_sf(x = df, coords = c('lon', 'lat'), crs = 4326)

## Visualise
tmap_mode('view')
tm_shape(tmp_sf) + tm_dots(col = 'price_integer')

## Search
tmp <- df[which(df$price_integer < 600000),] %>%
  select(price_integer, Title, Address, Suburb, Bedrooms, Bathrooms, Area, LandArea, PropertyType, ListingId) %>%
  as_tibble()
tmp[order(tmp$price_integer),]

## Transform to sf
tmp_sf <- st_as_sf(x = df$GeographicLocation %>% select(Longitude, Latitude), coords = c('Longitude', 'Latitude'), crs = 4326)
tmp_sf <- cbind(tmp_sf, df[c('RateableValue', 'price_integer')])

## Visualise
tmap_mode('view')
tm_shape(tmp_sf) + tm_dots(col = 'price_integer')



## Analysis
tmp <- df %>% select(
  price_integer,
  Suburb,
  Area,
  LandArea,
  Bathrooms,
  Bedrooms,
  PropertyType,
  IsNew)

break

a <- list(
  AsAt = "Date",
  PropertyId = "character",
  Address = "character",
  RegionId = "factor",
  Region = "factor",
  SuburbId = "factor",
  Suburb = "factor",
  District = "factor",
  DistrictId = "factor",
  Area = "numeric",
  LandArea = "numeric",
  Bathrooms = "factor",
  Bedrooms = "factor",
  Parking = "character",
  PropertyType = "factor",
  RateableValue = "numeric",
  IsNew = "boolean",
  Amenities = "character"
)

for(x in names(a)) {
  if(x %in% names(tmp)){
    if(a[[x]] == 'factor') {
      tmp[,x] <- tmp[,x] %>% as.character() %>% as.factor()
    } else if (a[[x]] == 'character') {
      tmp[,x] <- tmp[,x] %>% as.character()
    } else if (a[[x]] == 'numeric') {
      tmp[,x] <- tmp[,x] %>% as.character() %>% as.numeric()
    } else {}
  } else {}
}

fit1 <- lm(log(price_integer) ~ , data = tmp)
summary(fit1)
plot_model(fit1)

"Parking" ## This variable is a total mess
"Amenities" ## This variable is a total mess

"MemberId"
"AgencyReference"
"BestContactTime"
"ViewingInstructions"


Within Agency
"Id"
"Name"
"Agents"
"IsRealEstateAgency"
"IsLicensedPropertyAgency"

df or list variables
"OpenHomes"
"GeographicLocation"
"AdjacentSuburbNames"
"AdjacentSuburbIds"
"PhotoUrls"


"IsFeatured"
"HasGallery"
"IsBold"
"IsHighlighted"
"IsClassified"
"IsSuperFeatured"
"Has3DTour"
"HasEmbeddedVideo"
"IsBoosted"

"PictureHref"



