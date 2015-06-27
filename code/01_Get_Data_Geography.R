require(acs)
require(dplyr)
require(zipcode)


# Step 1 
# get Zipcodes in Nashville MSA 
nash.msa.file <- 'data/clean/nashville_zips.RData'

if(file.exists(nash.msa.file)){
  load(nash.msa.file)
} else{
  nash.msa.fips <- '34980'
  zip.msa.file <- 'data/raw/zcta_cbsa_rel_10.txt'
  
  # download census files
  if(!file.exists(zip.msa.file)) {
    download.file(
      'http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_cbsa_rel_10.txt',
      zip.msa.file)
  }
  
  # Get a list of zips in Nashville MSA
  zips.nash <- data.table::fread(zip.msa.file,
                                data.table = F, 
                                colClasses=list(character=1:2), 
                                integer64 = 'numeric') %>%
    select(zip=ZCTA5, msa_cd=CBSA, ZAREALANDPCT) %>%
    group_by(zip) %>%
    top_n(1, ZAREALANDPCT) %>%
    ungroup() %>%
    filter(
      msa_cd == nash.msa.fips
      & ZAREALANDPCT > 50) %>%
    select(zip) 
  
  save(zips.nash, file = nash.msa.file)
}


# Step 2
# Zip-City-State List
data(zipcode)

# Step 2
# get zip code geographic data
geo.file <- 'data/clean/geo.RData'

if(file.exists(geo.file)){
  load(geo.file)
} else {
  
  zip.gazetteer.file <- 'data/raw/2014_Gaz_zcta_national.zip'
  if(!file.exists(zip.gazetteer.file)) {
    download.file(
      'http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2014_Gazetteer/2014_Gaz_zcta_national.zip',
      zip.gazetteer.file)
  }
  tmp <- unzip(zip.gazetteer.file)
  zips.geo <- read.table(tmp,
                         skip = 1,
                         col.names = c('zip','','', 'area','', 'lat', 'long'),
                         header = T, sep = '\t',
                         colClasses = c('character','NULL','NULL',
                                        'numeric','NULL',
                                        'numeric','numeric'))
  file.remove(tmp)
  rm(tmp, zip.gazetteer.file)
  
  save(zips.geo, file = geo.file)
}



# Step 3
# Get demographic data
# from US CENSUS API - American Community Survey

acs.file <- 'data/clean/acs.RData'

if(file.exists(acs.file)){
  load(acs.file)
} else {
  # setup geography - all zip codes
  geo = geo.make(zip.code = '*')
  
  # download population
  acs.pop = acs.fetch(endyear = 2013, 
                      geo=geo, 
                      variable ="B01003_001",
                      col.names = 'pop')
  # download median personal income
  acs.inc = acs.fetch(endyear = 2013, 
                      geo=geo, 
                      variable ="B19013_001",
                      col.names = 'inc')
  
  stopifnot(identical(geography(acs.pop), geography(acs.inc)))
  
  zips.acs <- data.frame(
    zip = geography(acs.pop)[,2],
    pop = estimate(acs.pop),
    inc = estimate(acs.inc),
    stringsAsFactors = F,
    row.names = NULL) 
  
  save(zips.acs,file = acs.file)
}






# Step 6
# Combine all ZIP code data
zips <- zipcode %>% 
  # zip city state
  select(zip, city) %>%
  # subset to Nash MSA
  semi_join(zips.nash, by='zip') %>% 
  # demos
  left_join(zips.acs, by = 'zip') %>%
  # geography
  left_join(zips.geo, by = 'zip') %>%
  # calculate density
  mutate(dens = as.integer(pop/area))


# Step 6 
# Aggregate on Cities
cities <- zips %>%
  group_by(city) %>%
  summarize(
    zips = n(),
    inc  = as.integer(weighted.mean(inc,pop,na.rm = T)),
    pop = sum(pop,na.rm = T),
    area = sum(area,na.rm = T),
    lat = mean(lat,na.rm = T),
    long = mean(long,na.rm = T),
    dens = as.integer(sum(pop,na.rm = T)/sum(area,na.rm = T))) %>%
  ungroup()


# Cache geography data
save(zips, cities,
     file = 'data/clean/geography.RData')


