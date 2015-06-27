require(ggplot2)
require(scales)
require(leaflet)
require(geosphere)
require(DT)
require(dplyr)

load('data/clean/nash_restaurants.RData')



summary(nash$rating)

ggplot(nash, aes(x=rating)) + 
  ggtitle('Number of restaurants by Yelp Rating') +
  geom_histogram(binwidth = 0.50, fill='firebrick', color='white') +
  scale_x_continuous(limits=c(1, 5)) +
  scale_y_continuous(labels = comma)



# Normalize ratings
nash <- nash %>% 
  mutate(yelp_qrt = cut(rating, 
                        breaks = quantile(rating, seq(0,1,length.out = 5)), 
                        include.lowest = T,
                        labels = 1:4))
ggplot(nash, aes(x=yelp_qrt)) + 
  ggtitle('Number of restaurants by Yelp Rating Quartile') +
  geom_histogram(binwidth = 0.50, fill='firebrick', color='white') +
  scale_y_continuous(labels = comma)




# best hoods
df = nash.hood %>%
  filter(!is.na(neighborhood)) %>%
  arrange(-rating) %>% 
  mutate(rank = rank(-rating),
         rating = round(rating,2),
         popup = paste0(
           neighborhood,
           '<BR>Rank: ', rank, ' of ', n(),
           '<BR>Restaurants: ', count,
           '<BR>Avg Rating: ', rating,
           '<BR>Reviews: ', comma(review_count)))  

datatable(
  df %>% 
    select(rank, neighborhood, dist, count, rating, review_count) %>% 
    arrange(-rating) %>% 
    mutate(review_count = comma(review_count)), 
  colnames = c('Rank','Neighborhood',
               'Distance', 'Restaurants', 'Rating', 'Reviews'),
  class = 'row-border stripe compact order-column hover',
  options=list(
    pageLength = 15, lengthMenu = c(5, 10, 15, 20, 50),
    columnDefs = list(list(className = 'dt-right', targets = c(5)),
                      list(className = 'dt-center', targets = c(0)))),
  rownames=FALSE)


# Create  ratings quantile-based discrete palette function
pal <- colorQuantile(
  palette = c('tomato', 'yellow', 'forestgreen'),
  domain = df$rating,
  n=5
)

agaIcon <- icons(
  iconUrl = "img/aga.png",
  iconWidth = 422/5.626667, iconHeight = 233/5.626667)


nash.view = list(lat =36.15970,
                 lng = -86.76060,
                 zoom = 12)

leaflet(data = df, width = 900, height = 450) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = nash.view$lng, lat = nash.view$lat, zoom = nash.view$zoom) %>% 
  addCircleMarkers(
    popup = ~popup,
    lng = ~long, lat = ~lat,
    radius = ~sqrt(count)*2,
    color = ~pal(rating),
    stroke = FALSE, fillOpacity = 0.50) %>% 
  addMarkers(lng = -86.69255, lat = 36.21149, 
             icon = agaIcon,
             popup = 'AGA Conference<BR>Gaylord Opryland Hotel & Convention Center')



# best zips
df = nash.zip %>%
  filter(!is.na(zip)) %>%
  arrange(-rating) %>% 
  mutate(rank = rank(-rating),
         popup = paste0(
           'Zip Code: ', zip,
           '<BR>Rank: ', rank, ' of ', n(),
           '<BR>Restaurants: ', count,
           '<BR>Avg Rating: ', round(rating,2),
           '<BR>Reviews: ', comma(review_count)))  

# Create  ratings quantile-based discrete palette function
pal <- colorQuantile(
  palette = c('tomato', 'yellow', 'forestgreen'),
  domain = df$rating,
  n=5
)

leaflet(data = df, width = 900, height = 450) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = nash.view$lng, lat = nash.view$lat, zoom = nash.view$zoom) %>% 
  addCircleMarkers(
    popup = ~popup,
    lng = ~long, lat = ~lat,
    radius = ~sqrt(count)*2,
    color = ~pal(rating),
    stroke = FALSE, fillOpacity = 0.50) %>% 
  addMarkers(lng = -86.69255, lat = 36.21149, 
             icon = agaIcon,
             popup = 'AGA Conference<BR>Gaylord Opryland Hotel & Convention Center')



# Best Restaurants
df = nash %>%
  arrange(-rating) %>% 
  mutate(rank = rank(-rating),
         popup = name)  

# Create  ratings quantile-based discrete palette function
pal <- colorQuantile(
  palette = c('tomato', 'yellow', 'forestgreen'),
  domain = df$rating,
  n=5
)

leaflet(data = df, width = 900, height = 450) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    popup = ~popup,
    lng = ~lng, lat = ~lat,
    radius = ~sqrt(count)*3,
    color = ~pal(rating),
    stroke = FALSE, fillOpacity = 0.50) %>% 
  addMarkers(lng = -86.69255, lat = 36.21149, 
             icon = agaIcon,
             popup = 'AGA Conference<BR>Gaylord Opryland Hotel & Convention Center')



