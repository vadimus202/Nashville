library(httr)
library(jsonlite)
library(dplyr)

source("../setup_uber_oauth.R")
stopifnot(exists('uber_token'))

aga.addr = '2800 Opryland Drive, Nashville, TN 37214'
aga.loc <- ggmap::geocode(location = aga.addr)
aga.loc <- rev(aga.loc)




fares <- GET("https://api.uber.com/v1/products",
             query=list(
                 server_token = uber_token,
                 latitude = aga.loc$lat,
                 longitude = aga.loc$lon))

fares <- content(fares)
fares <- fromJSON(toJSON(fares))
fares



load("data/clean/nash_restaurants.RData")


z <- GET("https://api.uber.com/v1/estimates/price",
         query=list(
             server_token = uber_token,
             start_latitude = aga.loc$lat,
             start_longitude = aga.loc$lon,
             end_latitude = nash.hood$lat[1],
             end_longitude = nash.hood$long[1]
         ))
z <- content(z)
z <- fromJSON(toJSON(z))$prices
z



uber.raw <- apply(
    X = filter(nash.hood, !is.na(neighborhood)), 
    MARGIN = 1, 
    FUN = function(x){
        z <- GET("https://api.uber.com/v1/estimates/price",
                 query=list(
                     server_token = uber_token,
                     start_latitude = aga.loc$lat,
                     start_longitude = aga.loc$lon,
                     end_latitude = x['lat'],
                     end_longitude = x['long']
                 ))
        z <- content(z)
        z <- fromJSON(toJSON(z))$prices
        df <- data.frame(
            hood = x['neighborhood'],
            product = unlist(z$localized_display_name),
            duration = as.integer(unlist(z$duration)/60),
            distance = unlist(z$distance),
            estimate = unlist(z$estimate),
            stringsAsFactors = F, row.names = NULL)
        df
    })

uber <- do.call("rbind", uber.raw)

save(fares, uber,
     file = 'data/clean/nash_uber.RData')
