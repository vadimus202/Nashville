library(dplyr)

load('data/clean/nash_restaurants.RData')

source('code/function_Yelp_API_TermSearch.R')
source('code/function_Uber_API.R')
source('code/function_Bayes_Avg.R')

# place uber token in global env
source("../setup_uber_oauth.R")
stopifnot(exists('uber_token'))

# distance to AGA
aga.addr = '2800 Opryland Drive, Nashville, TN 37214'
aga.loc <- ggmap::geocode(location = aga.addr)
stopifnot(length(aga.loc)==2)

# call Yelp API
hot.chic.yelp <- lkp_yelp_by_category(term = 'hot chicken', 
                                      category_filter = 'restaurants', 
                                      location = 'Nashville, TN')

# review results
hot.chic.yelp$errors
hot.chic.yelp$elapsed.min
hot.chic <- hot.chic.yelp$results


# scrape Yelp website for reviews and count search term matches
review_match <- function(url, pattern){
    library(XML)
    
    sapply(url, function(x){
        doc <- htmlTreeParse(x, useInternalNodes = T)
        reviews <- xpathApply(doc, "//p[@itemprop='description']", xmlValue)
        sum(grepl(pattern, unlist(reviews), ignore.case = T))
    })
    
}


hot.chic$matches <- review_match(url = hot.chic$url, 
                                 pattern = '(hot|spicy) chicken')

hot.chic <- hot.chic %>% 
    # keep records with more than one mention in reviews 
    filter(matches>1L)

# add weighted ratings-based ranks
hot.chic <- hot.chic %>% 
    mutate(prc = rank(matches)/length(matches),
           adj_votes = review_count*prc) %>% 
    rename(orig_votes = review_count,
           review_count = adj_votes)

hot.chic <- get_bayes_avg(hot.chic)

hot.chic <- hot.chic %>% 
    rename(adj_votes=review_count, 
           review_count=orig_votes) %>% 
    # use hood from original data
    select(-neighborhood) %>% 
    left_join(
        select(nash, id, neighborhood),
        by='id')

# add Uber info
df.uber <- hot.chic %>% 
    mutate(lat1 = as.numeric(aga.loc$lat),
           lon1 = as.numeric(aga.loc$lon)) %>% 
    select(id, lat1, lon1, lat2=lat, lon2=lng)

df.uber <- get_uber_estimates(df = df.uber, uber_token = uber_token)

hot.chic <- left_join(hot.chic, df.uber, by='id')


# cache results
save(hot.chic, file = 'data/clean/nash_hot_chicken.RData')
