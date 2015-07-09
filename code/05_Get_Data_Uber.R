library(dplyr)

# place uber token in global env
source("../setup_uber_oauth.R")
stopifnot(exists('uber_token'))

# source Uber functions
source("code/function_Uber_API.R")


aga.addr = '2800 Opryland Drive, Nashville, TN 37214'
aga.loc <- ggmap::geocode(location = aga.addr)
stopifnot(length(aga.loc)==2)

uber.prod <- get_uber_products(lat = aga.loc$lat, lon = aga.loc$lon, uber_token = uber_token)


load("data/clean/nash_restaurants.RData")


df <- nash.hood %>% 
    filter(!is.na(neighborhood)) %>% 
    mutate(lat1 = as.numeric(aga.loc$lat),
           lon1 = as.numeric(aga.loc$lon)) %>% 
    select(id = neighborhood, lat1, lon1, lat2=lat, lon2=lon)

nash.hood.uber <- get_uber_estimates(df = df, uber_token = uber_token)

save(uber.prod, nash.hood.uber, file = 'data/clean/nash_uber.RData')
