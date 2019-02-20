require(jsonlite)
setwd('~/Desktop/scraping/trademe')
overview <- fromJSON('tradeMeOverview.json')
detail <- fromJSON('tradeMeDetail.json')
names(detail) <- gsub(':','',names(detail))
names(detail) <- gsub('#','',names(detail))
data <- merge(overview, detail, by = 'Link')

save(data, file='data.Rda')