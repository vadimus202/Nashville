
nash$categories <- gsub(':', ', ', nash$categories)


nash %>% 
  group_by(categories) %>% 
  summarize(count = n(),
            dist = round(weighted.mean(dist, review_count),2),
            rating = round(weighted.mean(rating, review_count),2),
            review_count = sum(review_count)) %>%   top_n(20,rating*review_count) %>% 
  arrange(-rating*review_count) %>%
  mutate(Rank = row_number()) %>% 
  select(Rank, name, neighborhood, categories ,rating, review_count,dist)



# TOP 20
######################################
datatable(
  nash %>% 
    top_n(20,rating*review_count) %>% 
    arrange(-rating, -review_count) %>%
    mutate(Rank = row_number()) %>% 
    select(Rank, name, neighborhood, categories ,rating, review_count,dist),
  colnames = c('Rank','Name','Neighborhood', 'Category', 'Rating', 'Reviews','Distance'),
  class = 'row-border stripe compact order-column hover',
  options=list(pageLength = 20, dom = 't'),
  rownames=FALSE
)

leaflet(data = nash %>% 
          top_n(20,rating*review_count) %>% 
          arrange(-rating, -review_count) %>%
          mutate(Rank = row_number(),
                 popup = paste0(
                   name, 
                   '<BR>Rank: ', Rank, 
                   '<BR>Neighborhood: ', neighborhood, 
                   '<BR>Categories: ', categories, 
                   '<BR>Rating: ', rating, 
                   '<BR>Reviews: ', review_count, 
                   '<BR>Distance: ', dist
                 )) %>% 
          select(popup, lat, lng), 
        width = 900, height = 450) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = nash.view$lng, lat = nash.view$lat, zoom = nash.view$zoom) %>% 
  addMarkers(popup = ~popup, lng = ~lng, lat = ~lat) %>% 
  addMarkers(lng = -86.69255, lat = 36.21149, 
             icon = agaIcon,
             popup = 'AGA Conference<BR>Gaylord Opryland Hotel & Convention Center')








# Best by Category
######################################

categ.best <- 'breweries|beer'

datatable(
  nash %>% 
    filter(grepl(categ.best, categories_cd)
           & !is.na(neighborhood)) %>%
    group_by(neighborhood) %>% 
    summarize(count = n(),
              dist = round(weighted.mean(dist, review_count),2),
              rating = round(weighted.mean(rating, review_count),2),
              review_count = sum(review_count)) %>% 
    arrange(-rating*review_count) %>% 
    filter(row_number()<=5),
  colnames = c('Neighborhood','Restaurants', 'Distance','Rating', 'Reviews'),
  class = 'row-border stripe compact order-column hover',
  options=list(pageLength = 5, dom = 't'),
  rownames=FALSE
)

datatable(
  nash %>% 
    filter(grepl(categ.best,categories_cd)) %>% 
    top_n(20,rating*review_count) %>% 
    arrange(-rating*review_count) %>%
    mutate(Rank = row_number()) %>% 
    select(Rank, name, neighborhood, categories ,rating, review_count,dist),
  colnames = c('Rank','Name','Neighborhood', 'Category', 'Rating', 'Reviews','Distance'),
  class = 'row-border stripe compact order-column hover',
  options=list(pageLength = 20, dom = 't'),
  rownames=FALSE
)

leaflet(data = nash %>% 
          filter(grepl(categ.best,categories_cd)) %>%
          top_n(20,rating*review_count) %>% 
          arrange(-rating*review_count) %>%
          mutate(Rank = row_number(),
                 popup = paste0(
                   name, 
                   '<BR>Rank: ', Rank, 
                   '<BR>Neighborhood: ', neighborhood, 
                   '<BR>Categories: ', categories, 
                   '<BR>Rating: ', rating, 
                   '<BR>Reviews: ', review_count, 
                   '<BR>Distance: ', dist
                 )) %>% 
          select(popup, lat, lng), 
        width = 900, height = 450) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = nash.view$lng, lat = nash.view$lat, zoom = nash.view$zoom) %>% 
  addMarkers(popup = ~popup, lng = ~lng, lat = ~lat) %>% 
  addMarkers(lng = -86.69255, lat = 36.21149, 
             icon = agaIcon,
             popup = 'AGA Conference<BR>Gaylord Opryland Hotel & Convention Center')


