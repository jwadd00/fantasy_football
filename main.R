# libraries
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(tidyverse, rvest, lubridate, here, glue)
#pacman::p_load(rvest, lubridate, reshape, tidyverse, future.apply, stringi, jsonlite)

# here for relative path / project
path <- here::here()

# run dfs scraper



