
## StatsNZ NZ.Stat
library(tidyverse)
library(httr)
library(jsonlite)


# function to call the stats nz open data catalogue

get_odata_catalogue <-  function(service, endpoint, service_api_key) {

  catalogue_url <- URLencode(paste0(service, "/", endpoint))

  # Add the proxy authentication
  config_proxy <- use_proxy(
    url = curl::ie_get_proxy_for_url(service),
    auth = "any",
    username = ""
  )

  # Look at the available tables
  opendata_catalogue <-
    GET(
      url = catalogue_url,
      config_proxy,
      add_headers(.headers = c('Cache-Control' = 'no-cache',
                               'Ocp-Apim-Subscription-Key' = service_api_key)),
      timeout(60)
    ) %>%
    content(as = "text") %>%
    fromJSON()

  opendata_catalogue <- as.data.frame(opendata_catalogue$dataset) %>%
    unnest_longer(distribution)


  structure(opendata_catalogue,
            comment = "Odata Catalogue")

}



# function to call the stats nz open data api

get_odata <-  function(service, endpoint, entity, query_option, service_api_key) {

  config_proxy <- use_proxy(
    url = curl::ie_get_proxy_for_url(service),
    auth = "any",
    username = ""
  )

  odata_url <- URLencode(paste0(service, "/", endpoint, "/", entity, "?", query_option))
  top_query <- grepl("$top",query_option,fixed=TRUE)

  # continue getting results while there are additional pages

  while (!is.null(odata_url)) {

    result <- GET(odata_url,
                  config_proxy,
                  add_headers(.headers = c("Content-Type" = "application/json;charset=UTF-8",
                                           "Ocp-Apim-Subscription-Key" = service_api_key)),
                  timeout(60)
    )


    # catch errors

    if (http_type(result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }


    if (http_error(result)) {
      stop(
        sprintf(
          "The request failed - %s \n%s \n%s ",
          http_status(result)$message,
          fromJSON(content(result, "text"))$value,
          odata_url
        ),
        call. = FALSE
      )
    }

    # parse and concatenate result while retaining UTF-8 encoded characters

    parsed <- jsonlite::fromJSON(content(result, "text", encoding = "UTF-8"), flatten = TRUE)
    response  <- rbind(parsed$value, if(exists("response")) response)
    odata_url <- parsed$'@odata.nextLink'


    cat("\r", nrow(response), "obs retrieved")

    # break when top(n) obs are specified

    if (top_query) {
      break
    }

  }

  structure(response,
            comment = "Odata response")

}








url <- 'https://api.stats.govt.nz/opendata/v1/data.json'
tmp <- GET(url, add_headers('Cache-Control' = 'no-cache', 'Ocp-Apim-Subscription-Key' = 'e7828daf64a84f45abde4b982ccee57a'))
tmp_content <- fromJSON(content(tmp, as = 'text'))
source('statsnz.env')





Catalogue <- get_odata_catalogue(
  service="https://api.stats.govt.nz/opendata/v1",
  endpoint="data.json",
  service_api_key = api_key
)

print(Catalogue[,c("distribution")])








ServiceEntities <-  Filter(function(x)!all(is.na(x)),
                           get_odata(
                             service = "https://api.stats.govt.nz/opendata/v1",
                             endpoint = "EmploymentIndicators",
                             entity = "",
                             query_option = "",
                             service_api_key = api_key))

print(ServiceEntities)


Resources <-  Filter(function(x)!all(is.na(x)),
                     get_odata(
                       service = "https://api.stats.govt.nz/opendata/v1",
                       endpoint = "EmploymentIndicators",
                       entity = "Resources",
                       query_option = "$select=ResourceID,Title,Var1,Var2,Modified,Frequency&$top=10",
                       service_api_key = api_key))

print(Resources)

Observations <-  Filter(function(x)!all(is.na(x)),
                        get_odata(
                          service = "https://api.stats.govt.nz/opendata/v1",
                          endpoint = "EmploymentIndicators",
                          entity = "Observations",
                          query_option = "$select=ResourceID,Period,Duration,Label1,Label2,Value,Unit,Measure,Multiplier&$top=10",
                          service_api_key = api_key))
print(Observations)


Observations <-  Filter(function(x)!all(is.na(x)),
                        get_odata(
                          service = "https://api.stats.govt.nz/opendata/v1",
                          endpoint = "EmploymentIndicators",
                          entity = "Observations",
                          query_option = "$filter=(
                                                    ResourceID eq 'MEI1.1' and
                                                    Period ge 2020-08-31 and
                                                    Label2 eq 'Actual' and
                                                    Duration eq 'P1M'
                                                  )
                                          &$select=ResourceID,Period,Duration,Label1,Label2,Value,Unit,Measure,Multiplier
                                          &$top=10",
                          service_api_key = api_key))

print(Observations)

Observations <-  Filter(function(x)!all(is.na(x)),
                        get_odata(
                          service = "https://api.stats.govt.nz/opendata/v1",
                          endpoint = "EmploymentIndicators",
                          entity = "Observations",
                          query_option = "$filter=(
                                                    ResourceID eq 'MEI1.1' and
                                                    Period ge 2020-08-31 and
                                                    Duration eq 'P1M'
                                                  )
                                          &$apply=groupby((Label1,Label2,Measure))
                                          &$top=10",
                          service_api_key = api_key))
print(Observations)

ggplot(data = tmp, aes(Period, Value)) + geom_line(aes(group = Label1, col = Label1))
