# libraries
if(!require(pacman)) install.packages('pacman')
pacman::p_load(tidyverse, rvest, nflfastR, lubridate, here, glue, janitor, furrr, tictoc, jsonlite)

# here for relative path / project
path <- here::here()

# run dfs scraper
source('./src/get_daily_fantasy_logs.R')

# run weather scraper
source('./src/get_weather_data.R')

# get box score data
source('./src/get_box_scores.R')

# run vegas scraper
source('./src/get_vegas_data.R')

# run receiving targets scraper
source('./src/get_receiving_targets.R')

# run red zone receiving targets scraper
source('./src/get_rz_receiving_targets.R')

# 