# load packages
library(rvest)
library(tidyverse)
library(future.apply)
library(glue)

# search this page for team URLs
urls <- 'https://www.covers.com/sport/football/nfl/teams' %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_nodes("tr") %>%
  html_nodes("a") %>%
  html_attr("href")

# parse teams out of url list
team_list <- map(.x = 1:32,
                 ~ gsub("/.*", "", substr(urls[.x],32, nchar(urls[.x])))
                 )

# set up partial url strings
url_string1 <- 'https://www.covers.com/sport/football/nfl/teams/main/'
url_string2 <- '/'

# set up function to generate full url's
url_gen <- function(t,y) {
  full_url <- paste0(url_string1,t,url_string2,y,'-',y+1)
}

# cartesian product of teams and all years
years <- c(2010:2020)
arguments <- expand.grid(t = team_list, y = years)

# use apply to pass cartesian product of teams and years through a function expecting two arguments
full_url_strings <- apply(arguments, 1, function(x) do.call(url_gen, as.list(x)))

# scrape function to scrape and parse data
#full_url_strings <- full_url_strings[1:3]

scrape_vegas <- function(x) {
  
  raw <- x %>% 
    read_html() %>% 
    html_nodes(css = '#TP_pastResults > div > div > div.covers-CoversTeams-pastResultsContainer.row > div > div > div > div > div > div > table') %>%
    html_table(fill=TRUE) %>% 
    purrr::keep(~ ncol(.x) == 7) %>% 
    purrr::keep(~ colnames(.x)[1] == "Regular Season") %>% 
    .[[1]] %>% 
    .[-1,]
  
  colnames(raw) <- c('date', 'vs', 'score', 'week', 'ats', 'o_u', 'none')
  
  df <- raw %>% 
    mutate(team = gsub("/.*", "", substr(x,54, nchar(x))),
           week = trimws(gsub("[^0-9.-]", "", week)),
           year = as.numeric(substr(x, nchar(x) - 3, nchar(x))) - 1,
           line = sub(".*? (.+)", "\\1", ats),
           o_u = sub(".*? (.+)", "\\1", o_u)) %>%
    select(team, year, week, line, o_u)
}

# run scrape function for all url's that were created
#plan(multiprocess)
vegas_list <-full_url_strings %>% 
  map_dfr(scrape_vegas)

