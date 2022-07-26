# load packages
library(jsonlite)
library(tidyverse)
library(future.apply)

# years
years <- c(2019)

# partial url strings
url_one <- 'http://api.airyards.com/'
url_two <- '/weeks'

# grab air yards data from air yards dot com
df_list <- future_lapply(years, function(x) 
  fromJSON(paste0(url_one,x,url_two)) %>%
    mutate(season = x) %>%
    select(full_name, season, week, tar, yac, air_yards, aypt, racr, target_share, wopr, tm_att, team_air)
  )

air_yards <- do.call(rbind, df_list)


fromJSON(paste0(url_one,'2019',url_two)) %>% glimpse()
