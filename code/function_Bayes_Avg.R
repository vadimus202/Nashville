get_bayes_avg <- function(df, min_votes=NULL){
    # Bayesian estimate
    # weighted rating (WR) = (v ÷ (v+m)) × R + (m ÷ (v+m)) × C
    # R = average for the movie (mean) = (Rating)
    # v = number of votes for the item = (votes)
    # m = minimum votes required to be listed in the Top List
    # C = the mean vote across the whole report
    
    
    # df requires the following 2 fields:
    stopifnot((
        is.null(df$rating) + is.null(df$review_count)
    )==0)
    
    # minimum votes
    if(is.null(min_votes)){
        min_votes <- quantile(df$review_count, 0.10)
    }
    
    df <- df %>% 
        mutate(R = rating,
               v = review_count,
               m = min_votes,
               C = mean(rating),
               WR = (v / (v+m)) * R + (m / (v+m)) * C) %>% 
        filter(v >= m) %>%   
        arrange(-WR) %>%
        mutate(Rank = row_number(),
               WR = round(WR, 2)) %>% 
        select(-R,-v,-m, -C)
    
    
    return(df)
}