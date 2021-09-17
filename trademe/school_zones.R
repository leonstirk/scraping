
library(sp)
library(sf)
library(rgdal)
library(rvest)

## Go to educationcounts website
url <- 'https://www.educationcounts.govt.nz/data-services/school-enrolment-zones'
page <- read_html(url)
## Have a look for the link to the master enrolment file
links <- page %>% html_elements('a')
file_url <- links[str_which(links %>% 
                              html_attr('title'), 'School Enrolment Scheme Master')] %>% 
                              html_attr('href')

## Download the file, unzip it, and read it in
temp <- tempfile()
temp2 <- tempfile()
download.file(file_url,temp)
unzip(zipfile = temp, exdir = temp2)
dat <- readOGR(file.path(temp2, list.files(temp2)[list.files(temp2) %>% str_which('.TAB')]))
unlink(c(temp, temp2))

## Transform it into something I can work with
dat <- st_as_sf(spTransform(dat, sp::CRS("+init=epsg:4326")), crs = 4326)

## There are a bunch of invalid geometries that have to be tidied up
dat <- st_make_valid(dat)
dat_valid <- dat[st_is_valid(dat),]
## Tidying doesn't work on some of them
dat_invalid <- dat[!st_is_valid(dat),]

## Plot the valid polygons
tmap_mode('view')
tm_shape(dat_valid) + tm_polygons(col = 'INSTTYPE', alpha = 0.2)

## List the invalid and therefore unplotted polygons
dat_invalid %>% as.data.frame() %>% .[,'PolyName']

