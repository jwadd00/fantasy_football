#### load packages ####
library(rvest)
library(tidyverse)
library(reshape)
library(future.apply)

# partial url strings
url1 <- "http://rotoguru1.com/cgi-bin/fyday.pl?week="
url2 <- "&year="
url3 <- "&game=dk&scsv=1"

# function to pass weeks and years through for full url generation
generateURL <- function(w,y) {
  full_url <- paste(url1,w, url2, y, url3, sep = '')
}

# weeks and years
weeks <- c(1:16)
years <- c(2018)
weeks_and_years <- expand.grid(w = weeks, y = years) # cartesian product of weeks and years

# generate all urls using a two argument function / pass cartesian product of weeks and years through function
url_list <- apply(weeks_and_years, 1, function(x) do.call(generateURL, as.list(x)))
rm(weeks_and_years)

# demonstrate urls
# url_list[1:3]

# function to scrape data
scrape_guru <- function(x) {
  page <- read_html(x) %>%
    html_nodes('pre') %>%
    html_text() %>%
    strsplit(split = '\n') %>%
    unlist() %>%
    .[. != ""] %>%
    colsplit(., split = ';', names = c('week', 'year','gid','player','pos','team','h_a','opp','dkp','salary')) %>%
    .[-1,]
}  

read_html(url_list[1]) %>%
  html_nodes('pre') %>%
  html_text() %>%
  strsplit(split = '\n') %>%
  unlist() %>%
  .[. != ""] %>%
  colsplit(., split = ';', names = c('week', 'year','gid','player','pos','team','h_a','opp','dkp','salary')) %>%
  .[-1,] %>% 
  as_tibble()

# execute scrape function for every url
plan(multiprocess)
guru_data_lapply <- future_lapply(url_list, function(x) scrape_guru(x))

# map to dataframe
guru_data_combined <- do.call(rbind, guru_data_lapply)
rm(guru_data_lapply)

# columns to convert to numeric
numeric_columns <- c('dkp','salary')

dfs_logs_nfl <- guru_data_combined %>% 
  mutate_if(is.factor, as.character) %>% # convert all columns from factor to character
  mutate_at(.vars = vars(numeric_columns), # convert numeric columns to numeric
            .funs = funs(as.numeric))

rm(guru_data_combined)
  

