library(dplyr)

# source functions
source("code/function_Uber_API.R")
source('code/function_Yelp_API_TermSearch.R')
source('code/function_Bayes_Avg.R')

# place uber token in global env
source("../setup_uber_oauth.R")
stopifnot(exists('uber_token'))


# distance to AGA
aga.addr = '2800 Opryland Drive, Nashville, TN 37214'
aga.loc <- ggmap::geocode(location = aga.addr)
stopifnot(length(aga.loc)==2)


# call Yelp API
music.yelp <- lkp_yelp_by_category(term = '', 
                                   category_filter = 'jazzandblues,musicvenues', 
                                   location = 'Nashville, TN',
                                   max.results = 200)

# review results
music.yelp$errors
music.yelp$elapsed.min
nash.music <- music.yelp$results 

nash.music <- get_bayes_avg(nash.music, min_votes = 35)

# add Uber info
df.uber <- nash.music %>% 
    mutate(lat1 = as.numeric(aga.loc$lat),
           lon1 = as.numeric(aga.loc$lon)) %>% 
    select(id, lat1, lon1, lat2=lat, lon2=lng)

df.uber <- get_uber_estimates(df = df.uber, uber_token = uber_token)

nash.music <- left_join(nash.music, df.uber, by='id')




# cache results
save(nash.music, file = 'data/clean/nash_music.RData')
