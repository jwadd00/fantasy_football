#### load packages ####
library(rvest)
library(tidyverse)
library(reshape)
library(future.apply)

# partial url strings
url_one <- "http://www.nflweather.com/en/week/"
url_two <- "/week-"
url_three <- "/"

# weeks, years and cartesian product of weeks and years to fill generate full urls
weeks <- c(1:2)
years <- c(2019:2019)
weather_weeks_and_years <- expand.grid(y = years, w = weeks) 

# function to pass weeks and years through for full url generation
generateWeatherURLs <- function(y,w) {
  full_weather_url <- paste(url_one,y,url_two,w,url_three,sep="")
}

# use apply to pass cartesian product of weeks and years through a function expecting two arguments
full_weather_url_strings <- apply(weather_weeks_and_years, 1, function(x) do.call(generateWeatherURLs, as.list(x)))

# function to scrape data 
scrapeWeather <- function (x) {
  scraped_weather <- x %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[1]/div[4]/div/div[1]/div[7]/div/table') %>%
    html_table() %>%
    .[[1]] %>%
    .[,c(2,6,10,12)] %>%
    mutate(id = paste(Away, Home, sep ='')) %>%
    melt(., id=c('id','Forecast','Wind')) %>%
    dplyr::rename(team = value, forecast = Forecast, wind = Wind) %>%
    select(team, forecast, wind) %>%
    mutate(year = str_sub(x, start= 35, end = 38),
           week = str_sub(sub('.*\\-', '', x), start = -3, end = -2),
           wind = as.numeric(gsub( "m.*$", "",wind)),
           temperature = ifelse(forecast == 'DOME', 71, as.numeric(gsub( "f.*$", "", forecast))),
           weather = sub(".*? (.+)", "\\1", forecast)) %>%
    select(year, week, team, weather, temperature, wind)
}


# pass urls through scrape function in parallel 
plan(multiprocess)
weather_data_list <- future_lapply(full_weather_url_strings, function(x) scrapeWeather(x))

# combine list of data to one data frame
weather_combined <- do.call(rbind, weather_data_list)

# package management
pacman::p_load(tidyverse, rvest, janitor)

# url / web page to scrape
url <- 'http://nflweather.com/en/week/2020/week-1/'

'http://nflweather.com/en/week/2020/week-1' %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]] %>% 
  clean_names() %>% View()
  select(away, home, forecast, wind) %>% 
  mutate(year = str_sub(url, start= 31, end = 34),
         week = str_sub(sub('.*\\-', '', url), start = -3, end = -2),
         wind = as.numeric(gsub( "m.*$", "",wind)),
         temperature = ifelse(forecast == 'DOME', 71, gsub( "f.*$", "", forecast)),
         weather = sub(".*? (.+)", "\\1", forecast)) %>% 
  pivot_longer(cols = c('away', 'home'), values_to = 'team') %>% 
  select(-name, -forecast) %>% 
  select(team, year, week, temperature, wind, weather) %>% 
  View()


# partial url strings
url_one <- "http://www.nflweather.com/en/week/"
url_two <- "/week-"
url_three <- "/"

# weeks, years and cartesian product of weeks and years to fill generate full urls
weeks <- c(1:17)
years <- c(2014:2020)
weather_weeks_and_years <- expand.grid(y = years, w = weeks) 

# function to pass weeks and years through for full url generation
generateWeatherURLs <- function(y,w) {
  full_weather_url <- paste(url_one,y,url_two,w,url_three,sep="")
}

full_url_strings <- map2(.x = weather_weeks_and_years$y, .y = weather_weeks_and_years$w, .f = ~ generateWeatherURLs(y = .x, w = .y))

# scrape function
scrapeWeather <- function(url) {
  scraped_weather <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]] %>% 
    clean_names() %>% 
    select(away, home, forecast, wind) %>% 
    mutate(year = str_sub(url, start= 35, end = 38),
           week = str_sub(sub('.*\\-', '', url), start = -3, end = -2),
           wind = as.numeric(gsub( "m.*$", "",wind)),
           temperature = ifelse(forecast == 'DOME', 71, gsub( "f.*$", "", forecast)),
           weather = sub(".*? (.+)", "\\1", forecast)) %>% 
    pivot_longer(cols = c('away', 'home'), values_to = 'team') %>% 
    select(-name, -forecast) %>% 
    select(team, year, week, temperature, wind, weather)
}

weather_data <- full_url_strings %>% 
  map_df(., scrapeWeather)
