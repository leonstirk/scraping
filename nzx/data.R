rm(list=ls())

require('jsonlite')
require('plyr')

## some functions

numify <- function(x) {
    return(as.numeric(gsub("[\\$%,]","",x)))
}

pvAnnuity <- function(C, r, t, m) {
    pv_annuity <- (C/r)*(1-(1/((1+(r/m))^(t*m))))
    return(pv_annuity)
}

pvLumpSum <- function(FV, r, t, m) {
    pv_lumpsum <- FV/exp((r/m)*(t*m))
    return(pv_lumpsum)
}

pvBond <- function(FV, C, r, t, m) {
    pv_bond <- pvAnnuity(C, r, t, m) + pvLumpSum(FV, r, t, m)
    return(pv_bond)
}

##############################################################

loadData <- function(filename) {
    ## Load data from JSON fromat
    data  <- fromJSON(filename)

    ## Reformat variable names
    names(data) <- gsub("[\\$\\(\\)\\/]","",names(data))
    names(data) <- gsub(" ","_",names(data))

    ## Convert all "-" to NA
    data[data == "-"] <- NA

    ## reformat timestamp
    data$data_access_timestamp <- strptime(data$data_access_timestamp, '%Y-%m-%d %T')

    return(data)
}

cleanBondData <- function(filename) {

    data <- loadData(filename)

    data$maturity_date[!is.na(data$`maturity_date_*`)] <- data$`maturity_date_*`[!is.na(data$`maturity_date_*`)]
    data$resettable_rate_bond <- sapply(data$`maturity_date_*`, function(x) { if(is.na(x)) {return(0)} else {return(1)} })

    ## Format date variables
    date_var_names <- c('maturity_date', 'previous_payment_date', 'next_record_date')
    data[date_var_names] <- lapply(data[date_var_names], function(x) { as.Date(x, '%d %b %Y') })

    ## face value
    data$face_value <- numify(data$face_value)

    ## price normalisation factor
    data$norm <- 1/data$face_value

    ## frequency -> number of periods per annum (m)
    data$m <- as.numeric(mapvalues(data$frequency, from = c("Half Yearly", "Quarterly"), to = c("2", "4")))

    ## time to maturity
    data$t <- numify(with(data, (maturity_date-next_record_date)/365))

    ## coupon
    data$coupon_rate <- NA
    data$coupon_payment <- NA
    data$coupon_rate <- numify(data$coupon)/100
    data$coupon_payment <- with(data, (coupon_rate/m)*face_value)

    ################################################################################################

    ## yield
    data$yield <- NA
    data[grep("%", data$price),]$yield <- data[grep("%", data$price),]$price
    data$yield <- numify(data$yield)/100
    data$yield_priced_instrument <- sapply(data$yield, function(x) { if(is.na(x)) {return(0)} else {return(1)} })

    ## price
    data[grep("[\\$]", data$price),]$price_per_100 <- numify(data[grep("[\\$]", data$price),]$price)*data[grep("[\\$]", data$price),]$norm*100
    data$price <- numify(data$price_per_100)/100

    ################################################################################################

    ## risk-free implied price
    ## term structure expectations implied price



    ## data$pv <- with(data, pvBond(face_value, coupon_payment, yield, t, m))

    ## ## outstanding
    ## data$outstanding_000s <- numify(data$outstanding_000s)*1000

    ## ## trades, volume, value
    ## data$trades <- as.numeric(data$trades)
    ## data$volume <- numify(data$volume)
    ## data$value <- numify(data$value)
    ## data$average_trade_volume <- NA
    ## data$average_trade_value <- NA
    ## data$average_trade_volume <- data$volume/data$trades
    ## data$average_trade_value <- data$value/data$trades

    ## ## minimum holding
    ## data$minimum_holding <- numify(data$minimum_holding)


    ## ##trading status, instrument type
    ## data$trading_status <- as.factor(data$trading_status)
    ## data$type <- as.factor(data$type)

    drops <- c("maturity_date_*", "coupon", "price_per_100")
    data <- data[,!(names(data) %in% drops)]

    return(data)
}

cleanStockData <- function(data) {

    numeric_vars <- c('high', 'low', 'open', 'price', 'high_bid', 'low_offer', 'trades', 'value', 'volume', 'capitalisation_000s', 'nta', 'pe', 'eps', 'gross_div_yield', 'securities_issued')
    for(i in numeric_vars) { data[,i] <- numify(data[,i]) }

    factor_vars <- c('type', 'trading_status')
    for(i in factor_vars) { data[,i] <- as.factor(data[,i]) }

    ## trades, volume, value
    data$trades <- as.numeric(data$trades)
    data$volume <- numify(data$volume)
    data$value <- numify(data$value)
    data$average_trade_volume <- NA
    data$average_trade_value <- NA
    data$average_trade_volume <- data$volume/data$trades
    data$average_trade_value <- data$value/data$trades

    ## work out some ratios that I want.

    return(data)
}
