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

lkp_yelp_by_category <- function(category_filter, terms){
  require(jsonlite)
  
  # define API call function
  call_api <- function(zip, yelpURL){

    df <- NULL
    end <- TRUE
    total <- NA
    
    # call API
    yelp <- GET(yelpURL, yelp.sig)
    yelp <- content(yelp)
    yelp <- jsonlite::fromJSON(toJSON(yelp)) 
    
    # check if any matches are returned
    if(is.null(yelp$businesses)){
      errors <- bind_rows(errors, data.frame(zip = zip))
      cat("Error ", zip, "\n")
      # check if zip is returned
    } else {
      total = yelp$total
      
      df <- data.frame(
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
        id = unlist(yelp$bus$id),
        name = unlist(yelp$bus$name),
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
                            function(x) paste(x[,1,1],collapse = ":")),
        categories_cd = sapply(yelp$businesses$categories, 
                               function(x) paste(x[,2,1],collapse = ":")),
        stringsAsFactors = F)
      
      end <- nrow(df)<20
      
      df <- filter(df, !(id %in% results.all$id))
    }
    
    return(list(df=df,end=end, total=total))
    
  } # end api function
  
  
  # prepare URLs search parameters
  param <- terms %>%
    unique() %>%
    mutate(
      radius = sqrt(area/pi),
      radius = radius*3,        # increase radius to be conservative
      radius = radius*1609.34,  # convert to meters
      radius = as.integer(radius),
      url = paste0("http://api.yelp.com/v2/search?",
                   "category_filter=", category_filter, 
                   "&location=", zip,
                   "&radius_filter=", radius,
                   "&sort=0&limit=20")) %>%
    select(zip,url)
  
  # initialize
  results.all <- NULL
  errors <- NULL
  
  # Start the clock!
  start.time <- proc.time()
  
  # main loop
  for(i in 1:nrow(param)){
    zip = param$zip[i]
    offset = 0
    yelpURL = paste0(param$url[i],'&offset=', offset)
    
    # initial api call
    results <- call_api(zip, yelpURL)
    results.all <- bind_rows(results.all, results$df)
    
    if(!results$end){
      # continue until no matching zipcodes returned
      while(!results$end & offset<1000){
        
        # add offset
        offset = offset+20
        yelpURL = paste0(param$url[i],'&offset=', offset)
        
        # subsequent api calls
        results <- call_api(zip, yelpURL)
        if(!is.null(results$df)) {
          results.all <- bind_rows(results.all, 
                                   anti_join(results$df,
                                             results.all,
                                             by='id'))}
      }      
    }
    
    msg <- paste(i, "of", nrow(param), 'Complete')
    cat('\r',msg)
    
  }
  
  # Stop the clock
  elapsed.min <- (proc.time() - start.time)[3]/60
  
  return(list(
    results = results.all,
    errors = errors,
    elapsed.min = elapsed.min))
  
  
}
