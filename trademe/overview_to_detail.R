require(jsonlite)

setwd('~/Desktop/scraping/trademe')

data <- fromJSON('tradeMeOverview.json')
links <- data$link
exportJson <- toJSON(links)
write(exportJson, file="overviewToDetail.json")

