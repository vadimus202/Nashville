require(geosphere)
require(dplyr)


# yelp search term
category_filter = 'restaurants'



load('data/clean/geography.RData')

# source functions
source("code/function_Uber_API.R")
source('code/function_Bayes_Avg.R')

# place uber token in global env
source("../setup_uber_oauth.R")
stopifnot(exists('uber_token'))


# distance to AGA
aga.addr = '2800 Opryland Drive, Nashville, TN 37214'
aga.loc <- ggmap::geocode(location = aga.addr)
stopifnot(length(aga.loc)==2)



#get Yelp API signature key
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



# Prep Final Output datasets

# restaurant-level data
nash <- nash.yelp$results %>%
    # remove fast food, food trucks, non-food
    filter(!grepl('foodtrucks|foodstands|streetvendors|hotdogs|cloth|laundry|accessories|antiques|
                      aquariums|arcades|artsandcrafts|bookstores|
                homedecor|movietheaters',categories_cd)) %>% 
    rename(zip = postal_code)

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
           count_per_sqmi = round(count/area*100,3)) %>%
    select(zip, city, count, count_per_person, 
           count_per_sqmi, rating, review_count, review_per_person,
           pop, inc, lat, long, area, dens) %>%
    arrange(-count)


nash.hood <- nash %>%
    filter(!is.na(neighborhood)) %>% 
    group_by(neighborhood) %>%
    summarize(
        count = n(),
        rating = weighted.mean(rating, review_count),
        review_count = sum(review_count)) %>%
    ungroup() 

# add ranks
nash.hood <- get_bayes_avg(nash.hood, min_votes = 500)

# get hood coord from Google API
hood.loc <- ggmap::geocode(
    location = paste(nash.hood$neighborhood, 
                     "Nashville, TN", sep = ', '))
nash.hood <- cbind(nash.hood, hood.loc)

# add Uber info
df.uber <- nash.hood %>% 
    mutate(lat1 = as.numeric(aga.loc$lat),
           lon1 = as.numeric(aga.loc$lon)) %>% 
    select(id=neighborhood, lat1, lon1, lat2=lat, lon2=lon)

df.uber <- get_uber_estimates(df = df.uber, uber_token = uber_token)

nash.hood <- left_join(nash.hood, df.uber, by=c('neighborhood'='id'))



# save
save(nash.yelp, 
     file = 'data/clean/nash_yelp.RData')
save(nash, 
     nash.zip, 
     nash.hood, 
     file = 'data/clean/nash_restaurants.RData')
