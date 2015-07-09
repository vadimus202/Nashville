require(geosphere)
require(dplyr)


# yelp search term
category_filter = 'restaurants'

# distance to AGA
addr = '2800 Opryland Drive, Nashville, TN 37214'
geo.loc <- ggmap::geocode(location = addr)
geo.loc <- c(geo.loc$lat, geo.loc$lon)




load('data/clean/geography.RData')



#get API signature key
source('../setup_yelp_oauth.R')
stopifnot(!is.null(yelp.sig))

# prep Yelp search terms
terms <- zips %>%
    filter(city=='Nashville') %>% 
    select(zip, area) %>%
    unique()               #%>% sample_n(5,replace = F,)

# Call Yelp API
source('code/function_Yelp_API.R')
nash.yelp <- lkp_yelp_by_category(category_filter, terms)
nash.yelp$elapse
nash.yelp$err


# Bayesian estimate
# weighted rating (WR) = (v ÷ (v+m)) × R + (m ÷ (v+m)) × C
# R = average for the movie (mean) = (Rating)
# v = number of votes for the item = (votes)
# m = minimum votes required to be listed in the Top List
# C = the mean vote across the whole report



# Prep Final Output datasets

# restaurant-level data
nash <- nash.yelp$results %>%
    # remove fast food, food trucks, non-food
    filter(!grepl('foodtrucks|foodstands|streetvendors|hotdogs|cloth|laundry|accessories|antiques|
                      aquariums|arcades|artsandcrafts|bookstores|
                homedecor|movietheaters',categories_cd)) %>% 
    rename(zip = postal_code) %>% 
    mutate(dist =  distCosine(geo.loc, cbind(lat, lng), r = 6378137*0.000621371192),
           dist = round(dist,2))

# aggregate by Zip code
nash.zip <- nash %>%
    group_by(zip) %>%
    summarize(count = n(),
              rating = weighted.mean(rating, review_count,na.rm = T),
              review_count = sum(review_count,na.rm = T)) %>%
    ungroup() %>%
    inner_join(zips, by ='zip') %>%
    mutate(count_per_person = as.integer(count/pop*100000),
           review_per_person = as.integer(review_count/pop*100000),
           count_per_sqmi = round(count/area*100,3),
           dist =  distCosine(geo.loc, cbind(lat, long), 
                              r = 6378137*0.000621371192),
           dist = round(dist,2)) %>%
    select(zip, city, count, count_per_person, 
           count_per_sqmi, rating, review_count, review_per_person,
           pop, inc, lat, long, area, dens, dist) %>%
    arrange(-count)

# aggregate by neighborhood
min_votes <- 500

nash.hood <- nash %>%
    filter(!is.na(neighborhood)) %>% 
    group_by(neighborhood) %>%
    summarize(
        count = n(),
        rating = weighted.mean(rating, review_count),
        review_count = sum(review_count)) %>%
    ungroup() %>% 
    mutate(R = rating,
           v = review_count,
           m = min_votes,
           C = mean(rating),
           WR = (v / (v+m)) * R + (m / (v+m)) * C) %>% 
    filter(v>=m & !is.na(neighborhood)) %>%   
    arrange(-WR) %>%
    mutate(Rank = row_number(),
           rating = round(rating, 2),
           WR = round(WR, 2)) %>% 
    select(-R,-v,-m, -C)

hood.loc <- ggmap::geocode(
    location = paste(nash.hood$neighborhood, 
                     "Nashville, TN", sep = ', '))

nash.hood <- cbind(nash.hood, hood.loc)

# save
save(nash.yelp, 
     file = 'data/clean/nash_yelp.RData')
save(nash, 
     nash.zip, 
     nash.hood, 
     file = 'data/clean/nash_restaurants.RData')
