library(tidyverse)
library(rvest)
library(snakecase)

## NZX Main Board URL
url <- 'https://www.nzx.com/markets/NZSX'

html <- rvest::read_html(url)
tbl <- html %>%  html_table() %>% .[[1]]

## Clean up
names(tbl) <- to_any_case(names(tbl), 'snake')
tbl$change <- tbl$change %>% str_remove_all('(\\s\\/.*)')
n <- c('price', 'change', 'volume', 'value', 'capitalisation', 'percentage_change', 'trade_count', 'market_capitalisation')
tbl[n] <- lapply(tbl[n], function(x) { x %>% str_remove_all('[\\%\\$,]') %>% as.numeric() })

## Attach access timestamp
tbl$access_timestamp <- Sys.time()

##Print
board <- tbl
board

##########################################################################################
## Individual listing scraping for MORE DATA
## https://www.nzx.com/instruments/AMP

tcks <- sample(board$code)
## tcks <- tcks[1:10]
insts <- list()
st <- Sys.time()
for(i in 1:length(tcks)) {
  page_url <- paste0('https://www.nzx.com/instruments/',tcks[i])
  html <- rvest::read_html(page_url)
  tbl <- html %>%  html_table()
  tbl <- do.call(rbind, tbl) %>% as.data.frame()
  tmp <- tbl[,2]
  names(tmp) <- tbl[,1]
  tmp["code"] <- tcks[i]
  insts[[i]] <- tmp
}
et <- Sys.time()
print(et - st)
rm(et,st)

inst_df <- do.call(rbind, insts) %>% as_tibble()
names(inst_df) <- to_any_case(names(inst_df), 'snake')
names(inst_df)[which(names(inst_df) == 'type')] <- 'type_b'
drop_names <- names(inst_df)[which(!names(inst_df) %in% c(names(board), 'capitalisation_000_s' ,'trades'))]
merged_df <- left_join(inst_df[,c('code', drop_names)], board, by = 'code')
n <- c('open', 'high', 'low', 'high_bid', 'low_offer', 'p_e', 'eps', 'nta', 'gross_div_yield', 'securities_issued')
merged_df[n] <- lapply(merged_df[n], function(x) { x %>% str_remove_all('[\\%\\$,]') %>% as.numeric() })
merged_df <- merged_df %>% mutate_at('gross_div_yield', function(x) { x/100 })

now <- to_any_case(as.character(merged_df$access_timestamp[1]), 'snake')
## Save
saveRDS(merged_df, paste0('datasets/nzsx_', now, '.rds'))

##########################################################################################

## Add to master file
master_df <- readRDS('datasets/nzsx_master.rds')
saveRDS(master_df, paste0('datasets/nzsx_master_', now, '.rds'))
master_df <- bind_rows(master_df, merged_df)
saveRDS(master_df, paste0('datasets/nzsx_master.rds'))

##########################################################################################