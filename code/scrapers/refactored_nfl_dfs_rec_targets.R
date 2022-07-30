#### load packages ####
library(rvest)
library(tidyverse)
library(future.apply)

# partial url strings
url_one <- "http://www.nflsavant.com/targets.php?week="
url_two <- "&ddlYear="
url_three <- "&rz=all&ddlTeam=&ddlPosition="

# weeks, years
weeks <- c(1:16)
years <- c(2014:2021)

# cartesian product of positions, weeks and years
pos_weeks_years <- expand.grid(w = weeks, y = years)

# function to generate full url strings
genFootballSavantURLs <- function(w,y) {
  full_url <- paste(url_one, w, url_two, y, url_three, sep = '')
}

# run cartesian product of positions, weeks and years through url generator
urls <- apply(pos_weeks_years, 1, function(x) do.call(genFootballSavantURLs, as.list(x)))

# function to scrape nfl savant for total receiving targets
getNFLSavantTargets <- function(x) {
  tryCatch({
  targets <- x %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table %>%
    as.data.frame(.) %>%
    mutate(week = sub('\\&ddlYe.*', '',sub('.*week=', '', x)),
           year = sub('\\&rz=a.*', '',sub('.*Year=', '', x))
           )
  
  names(targets) <- c('rank', 'name', 'team', 'pos', 'comp', 'targets', 'comp_pct', 'td', 'week', 'year')
  
  targets <- targets %>%
    select(rank, name, pos, comp, targets, td, week, year)
  })
}

# pass urls through scrape function in parallel 
plan(multiprocess)
total_receiving_targets_data_list <- future_lapply(urls, function(x) getNFLSavantTargets(x))

# combine list of data to one data frame
total_receiving_targets <- do.call(rbind, total_receiving_targets_data_list)
