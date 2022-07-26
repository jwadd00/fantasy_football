#### load packages ####
library(rvest)
library(tidyverse)
library(future.apply)

# partial url strings
url_one <- "http://www.nflsavant.com/targets.php?week="
url_two <- "&ddlYear="
url_three <- "&rz=redzone_rush&ddlTeam=&ddlPosition="

# positions, weeks, years
weeks <- c(1:6)
years <- c(2017:2018)

# cartesian product of positions, weeks and years
pos_weeks_years <- expand.grid(w = weeks, y = years)

# function to generate full url strings
genFootballSavantRedZoneRushURLs <- function(w,y) {
  full_url <- paste(url_one, w, url_two, y, url_three, 'RB', sep = '')
}

# run cartesian product of positions, weeks and years through url generator
urls <- apply(pos_weeks_years, 1, function(x) do.call(genFootballSavantRedZoneRushURLs, as.list(x)))

# function to scrape nfl savant for red zone rush attempts
getNFLSavantRedZoneRush <- function(x) {
  tryCatch({
  rz_rush_att <- x %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table %>%
    as.data.frame(.) %>%
    mutate(week = sub('\\&ddlYe.*', '',sub('.*week=', '', x)),
           year = sub('\\&rz=r.*', '',sub('.*Year=', '', x))
    )
  
  names(rz_rush_att) <- c('rank', 'name', 'team', 'pos', 'rz_rush_att', 'rz_td', 'week', 'year')
  
  rz_rush_att <- rz_rush_att %>%
    select(rank, name, pos, rz_rush_att, rz_td, week, year)
  })
}

# pass urls through scrape function in parallel 
plan(multiprocess)
red_zone_rush_attempts_data_list <- future_lapply(urls, function(x) getNFLSavantRedZoneRush(x))

# combine list of data to one data frame
red_zone_rush_attempts <- do.call(rbind, red_zone_rush_attempts_data_list)