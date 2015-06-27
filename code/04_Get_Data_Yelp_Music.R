library(dplyr)
library(geosphere)

source('code/function_Yelp_API_TermSearch.R')

# distance to AGA
addr = '2800 Opryland Drive, Nashville, TN 37214'
geo.loc <- ggmap::geocode(location = addr)
geo.loc <- c(geo.loc$lat, geo.loc$lon)


# call Yelp API
music.yelp <- lkp_yelp_by_category(term = '', 
                                   category_filter = 'jazzandblues,musicvenues', 
                                   location = 'Nashville, TN',
                                   max.results = 200)

# review results
music.yelp$errors
music.yelp$elapsed.min
nash.music <- music.yelp$results %>% 
  mutate(dist =  distCosine(geo.loc, 
                            cbind(lat, lng), 
                            r = 6378137*0.000621371192),
         dist = round(dist,2),
         rank = rating*review_count,
         rank = as.integer(rank(-rank))) %>% 
  arrange(rank)


# cache results
save(nash.music, file = 'data/clean/nash_music.RData')
