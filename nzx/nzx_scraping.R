library(tidyverse)
library(rvest)
library(snakecase)

## NZX Debt Board URL
url <- 'https://www.nzx.com/markets/NZDX'

html <- rvest::read_html(url)
tbl <- html %>%  html_table() %>% .[[1]]

names(tbl) <- to_any_case(names(tbl), 'snake')
## Clean up
