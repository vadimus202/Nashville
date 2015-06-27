# function that acccepts 
#   1. a YELP category filter i.e. sushi, pizza, etc 
#       see full list here    
#         https://www.yelp.com/developers/documentation/v2/all_category_list
#   2. list of locations as search location paprameters (only zip codes tested)
#   3. Search Radius in miles (defualts to 1 if ommitted)
# makes URLs to query YELP API 
# and returns a list of 
#   1. summary data frame one row per each location input
#   2. detailed data frame
#   2. list of API errors if any, NULL otherwise
#   3. total time elapsed in minutes

lkp_yelp_by_category <- function(category_filter, loc='Nashville, TN', miles=10){
  require(jsonlite)
  require(ggmap)
  
  
  # define API call function
  call_api <- function(yelpURL){
    # call API
    yelp <- GET(yelpURL, yelp.sig)
    yelp <- content(yelp)
    yelp <- jsonlite::fromJSON(toJSON(yelp)) 
    
    # check if any matches are returned
    if(is.null(yelp$businesses)){
      errors <- bind_rows(errors, data.frame(zip = zip))
      cat("Error ", zip, "\n")
      df <- NULL
      # check if zip is returned
    } else if(is.null(yelp$bus$loc$postal)){
      df <- NULL
    } else {
      df <- data.frame(
        id = unlist(yelp$bus$id),
        name = unlist(yelp$bus$name),
        address = sapply(yelp$bus$loc$address, 
                         function(x) ifelse(is.null(x),NA, 
                                            paste(x,collapse = "\n"))),
        city = sapply(yelp$bus$loc$city, 
                      function(x) ifelse(is.null(x),NA, x)),
        neighborhood = ifelse(is.null(yelp$bus$loc$neighborhoods), NA,
                              sapply(yelp$bus$loc$neighborhoods, 
                                     function(x) ifelse(is.null(x),NA, x))),
        state_code = unlist(yelp$bus$loc$state_code),
        postal_code = sapply(yelp$bus$loc$postal_code, 
                             function(x) ifelse(is.null(x),NA, x)),
        phone = sapply(yelp$bus$display_phone, 
                       function(x) ifelse(is.null(x),NA, 
                                          gsub('^\\+1-','',x))),
        rating = unlist(yelp$bus$rating),
        review_count = unlist(yelp$bus$review_count), 
        snippet_text = sapply(yelp$bus$snippet_text, 
                              function(x) ifelse(is.null(x),NA, x)),
        closed  = unlist(yelp$bus$is_closed),
        lat = sapply(yelp$bus$loc$coordinate$latitude, 
                     function(x) ifelse(is.null(x),NA, x)),
        lng = sapply(yelp$bus$loc$coordinate$longitude, 
                     function(x) ifelse(is.null(x),NA, x)),
        categories = sapply(yelp$businesses$categories, 
                            function(x) paste(x[,1,1],collapse = ", ")),
        categories_cd = sapply(yelp$businesses$categories, 
                               function(x) paste(x[,2,1],collapse = ":")),
        stringsAsFactors = F)
      
    }
    return(df)
  } # end api function
  
  
  # prepare URLs search parameters
  radius = as.integer(miles*1609.34)  # convert to meters
  url = paste0("http://api.yelp.com/v2/search?",
               "category_filter=", category_filter, 
               "&location=", URLencode(loc),
               "&radius_filter=", radius,
               "&sort=0&limit=20")
  
  # initialize
  results.all <- NULL
  errors <- NULL
  
  # Start the clock!
  start.time <- proc.time()
  
  for(offset in seq(0,980,by = 20)){
    # continue until no matching zipcodes returned
    
    print(offset)
    yelpURL = paste0(url,'&offset=', offset)
    
    # subsequent api calls
    results <- call_api(yelpURL)
    results.all <- bind_rows(results.all, results)
  }
  
  msg <- paste(i, "of", nrow(param), 'Complete')
  cat('\r',msg)
  
  
  # Stop the clock
  elapsed.min <- (proc.time() - start.time)[3]/60
  
  return(list(
    results = results.all,
    errors = errors,
    elapsed.min = elapsed.min))
  
  
}
