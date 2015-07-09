

get_uber_products <- function(lat, lon, uber_token){
    library(httr)
    library(jsonlite)
    library(magrittr)
    
    api.json <- GET("https://api.uber.com/v1/products",
                    query=list(server_token = uber_token,
                               latitude = lat, longitude = lon)) %>% content()
    
    api.list <- fromJSON(toJSON(api.json))$products
    
    
    attach(api.list)
    api.df <- data.frame(
        product = unlist(display_name),
        descr = unlist(description),
        capacity = unlist(capacity),
        flat_fee = unlist(price_details$base),
        cost_per_mile = unlist(price_details$cost_per_distance),
        cost_per_minute = unlist(price_details$cost_per_minute),
        stringsAsFactors = F) 
    return(api.df)
}




get_uber_estimates <- function(df, uber_token){
    
    # df requires the following 5 fields:
    stopifnot((
        is.null(df$id)+
            is.null(df$lat1)+
            is.null(df$lon1)+
            is.null(df$lat2)+
            is.null(df$lon2)
        )==0)
    
    
    library(httr)
    library(jsonlite)
    library(reshape2)
    library(dplyr)
    
    api.all <- NULL
    for(i in 1:nrow(df)){
        api.json <- GET("https://api.uber.com/v1/estimates/price",
                        query=list(
                            server_token = uber_token,
                            start_latitude = df$lat1[i],
                            start_longitude = df$lon1[i],
                            end_latitude = df$lat2[i],
                            end_longitude = df$lon2[i])
        ) %>% content()
        
        # json to list
        api.list <- fromJSON(toJSON(api.json))$prices
        
        # unlist
        api.df <- data.frame(
            id = df$id[i],
            product = unlist(api.list$localized_display_name),
            duration = as.integer(unlist(api.list$duration)/60),
            distance = unlist(api.list$distance),
            estimate = unlist(api.list$estimate),
            stringsAsFactors = F, row.names = NULL)
        
        # append df
        api.all <- rbind(api.all, api.df)
    }
    
    # ride info
    uber.ride <- api.all %>% 
        group_by(id) %>% 
        summarise(duration = mean(duration),
                  distance = mean(distance)) %>% 
        ungroup()

    # fare estimates
        uber.est <- melt(api.all, 
                 id.vars = c('id', 'product'), 
                 measure.vars = 'estimate') %>% 
        dcast(id~product)

    # join ride info and estimates
    uber <- inner_join(uber.ride, uber.est, by='id')   
    
    return(uber)
}


