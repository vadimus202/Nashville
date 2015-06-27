library(dplyr)
library(geosphere)

source('code/function_Yelp_API_TermSearch.R')

# distance to AGA
addr = '2800 Opryland Drive, Nashville, TN 37214'
geo.loc <- ggmap::geocode(location = addr)
geo.loc <- c(geo.loc$lat, geo.loc$lon)


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
  filter(matches>1L) %>% 
  # rank results by rating, review count and # of mentions
  mutate(dist =  distCosine(geo.loc, 
                            cbind(lat, lng), 
                            r = 6378137*0.000621371192),
         dist = round(dist,2),
         rank = rating*review_count*matches,
         rank = as.integer(rank(-rank))) %>% 
  arrange(rank)


# cache results
save(hot.chic, file = 'data/clean/nash_hot_chicken.RData')
