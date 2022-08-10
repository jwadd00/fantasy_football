# libraries
if(!require(pacman)) install.packages('pacman')
pacman::p_load(tidyverse, rvest, lubridate, here, glue, janitor, furrr, tictoc, jsonlite)

# here for relative path / project
path <- here::here()

# run dfs scraper
source('./src/get_daily_fantasy_logs.R')

# run weather scraper
source('./src/get_weather_data.R')
