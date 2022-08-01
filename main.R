# libraries
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(tidyverse, rvest, lubridate, here, glue, furrr, tictoc)
#pacman::p_load(rvest, lubridate, reshape, tidyverse, future.apply, stringi, jsonlite)

# here for relative path / project
path <- here::here()

# run dfs scraper
source('./src/get_daily_fantasy_logs.R')


