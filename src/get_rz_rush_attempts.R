# partial url strings
url_one <- Sys.getenv("FANTASY_FOOTBALL_RED_ZONE_RUSHING")
url_two <- "&ddlYear="
url_three <- "&rz=redzone_rush&ddlTeam=&ddlPosition="

# weeks, years and cartesian product of weeks 
weeks <- c(1:16)
years <- c(2014:2021)
weeks_and_years <- crossing(week = weeks, year = years) 

# function to generate URLs
generateURL <- function(week, year) {
  
  full_url <- paste(url_one, week, url_two, year, url_three, sep="")
}

# generate all URLs to scrape
url_list <- pmap(weeks_and_years, generateURL) %>% 
  unlist()

# function to rip targets data 
scrape_rz_rush_attempts <- function(url) {
  
  rz_rush_attempts <- url %>% 
    read_html() %>%
    html_nodes("table") %>%
    html_table() %>%
    as.data.frame(.) %>%
    mutate(week = sub('\\&ddlYe.*', '',sub('.*week=', '', url)),
           year = substr(sub('.*Year=', '', url), 1, 4)) %>% 
    clean_names() %>%
    select(year, week, name, team, pos, rushes, t_ds) %>% 
    rename(rz_rush_td = t_ds)
}

# set a delay to scrape url responsibly
rate <- rate_delay(pause = 3)

slow_scrape_rz_rush_attempts <- slowly(scrape_rz_rush_attempts, rate = rate)

# run scrape jobs across all cores
plan(multisession, workers = availableCores())

tic()
rz_rushing_logs <- future_map_dfr(url_list, slow_scrape_rz_rush_attempts)
toc()