# partial url strings
url_one <- Sys.getenv("FANTASY_FOOTBALL_WEATHER_URL")
url_two <- "/week-"
url_three <- "/"

# weeks, years and cartesian product of weeks 
weeks <- c(1:16)
years <- c(2014:2021)
weeks_and_years <- crossing(week = weeks, year = years) 

# function to generate URLs
generateURL <- function(week, year) {
  
  full_url <- paste(url_one, year, url_two, week, url_three, sep="")
}

# generate all URLs to scrape
url_list <- pmap(weeks_and_years, generateURL) %>% 
  unlist()

# function to scrape weather data website
scrape_weather <- function(url) {
  
df <- url %>% 
  read_html() %>% # ingest page
  html_nodes(xpath='/html/body/div[1]/div[4]/div/div[1]/div[7]/div/table') %>% # select section of html to scrape
  html_table() %>% # convert to table list
  .[[1]] %>% # select first and only table with content
  .[,c(2,6,10,12)] %>% # limit to specific columns by column index
  mutate(id = paste(Away, Home, sep ='')) %>% # create id column
  clean_names() %>% # clean up the column names
  pivot_longer(away:home) %>% # elongate table
  rename(team = value) %>% 
  mutate(year = str_sub(url, start= 35, end = 38), # extract season from url
         week = str_sub(sub('.*\\-', '', url), start = -3, end = -2), # extract week from url
         wind = as.numeric(gsub( "m.*$", "",wind)), # extract numerical value from wind column
         temperature = ifelse(forecast == 'DOME', 71, as.numeric(gsub( "f.*$", "", forecast))), # extract temp from forecast column
         weather = sub(".*? (.+)", "\\1", forecast)) %>% 
  select(team, year, week, forecast, wind, temperature, weather)
}

# set a delay to scrape url responsibly
rate <- rate_delay(pause = 3)

slow_weather_scrape <- slowly(scrape_weather, rate = rate)

# run scrape jobs across all cores
plan(multisession, workers = availableCores())

tic()
weather_logs <- future_map_dfr(url_list, slow_weather_scrape)
toc()
