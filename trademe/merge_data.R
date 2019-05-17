require(jsonlite)
setwd('~/Desktop/scraping/trademe')
overview <- fromJSON('tradeMeOverview.json')
detail <- fromJSON('tradeMeDetail.json')
names(detail) <- gsub(':','',names(detail))
names(detail) <- gsub('#','',names(detail))

names(detail) <- gsub(' ','_',names(detail))
names(overview) <- gsub(' ','_',names(overview))

data <- merge(overview, detail, by = 'link')

save(data, file='data.Rda')