categ.list <- c("southern","bbq","steak","breweries|beer")


library(dplyr)

load("data/clean/nash_restaurants.RData")

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





get_top_categs <- function(df, categ.list){
    top.all <- NULL
    for(categ.best in categ.list){
        top <- filter(df, grepl(categ.best, categories_cd))
        top$categ.best <- categ.best
        top <- get_bayes_avg(top, min_votes = 20)
        top.all <- rbind(top.all, top)
    }
    return(top.all)
}

top <- get_top_categs(nash, categ.list)


top.hood <- top %>% 
    filter(!is.na(neighborhood)) %>% 
    group_by(neighborhood, categ.best) %>% 
    summarize(count = n(),
              rating = round(weighted.mean(rating, review_count),2),
              review_count = sum(review_count),
              WR = mean(WR)) %>% 
    group_by(categ.best) %>% 
    arrange(-WR) %>% 
    filter(count>1) %>% 
    top_n(5, WR) %>% 
    ungroup()

top <- top %>% 
    group_by(categ.best) %>% 
    top_n(20, WR) %>% 
    ungroup()
    
# add Uber info
df.uber <- top %>% 
    mutate(lat1 = as.numeric(aga.loc$lat),
           lon1 = as.numeric(aga.loc$lon)) %>% 
    select(id, lat1, lon1, lat2=lat, lon2=lng) %>% 
    unique()

df.uber <- get_uber_estimates(df = df.uber, uber_token = uber_token)

top <- left_join(top, df.uber, by='id')


save(top, top.hood, 
     file="data/clean/nash_top_categ.RData")
