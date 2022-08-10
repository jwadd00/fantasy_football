##############################
### get team slugs for url ###
##############################
teams_page <- read_html(Sys.getenv("FANTASY_FOOTBALL_VEGAS_URL"))

# filter html tags to those containing urls and then filter to team slug pattern
teams <- teams_page %>% 
  html_nodes("a") %>% # section
  html_attr("href") %>% # get links on page
  .[grepl("/sport/football/nfl/teams/main", .)] %>% # keep url's matching this pattern
  map(~ sub(".*/", "", .x)) # extract all characters after last forward slash

# seasons
years <- c('2014-2015', '2015-2016', '2016-2017', '2017-2018', '2019-2020', '2020-2021', '2021-2022')

# cartesian product of 
teams_and_years <- crossing(team = teams, year = years)

# historical vegas data base url
base_url <- paste0(Sys.getenv("FANTASY_FOOTBALL_VEGAS_URL"), 'main')

# function to generate URLs
generateURL <- function(team, year) {
  
  full_url <- paste(base_url, team, year, sep = '/')
}

# generate all URLs to scrape
url_list <- pmap(teams_and_years, generateURL) %>% 
  unlist()

# scrape function
scrape_vegas <- function(url) {
  
  # serialize team
  team_slug <- gsub(paste0(base_url, '/'), '', url) %>% sub('\\/.*', '', .) # extract team slug from url
  
  # serialize season
  season <- gsub(base_url, '', url) %>% sub('.*/', '', .) # extract season from end of url

  df <- read_html(url) %>% # read html from page
    html_node("#TP_pastResults") %>% # section
    html_table(fill=TRUE) %>% # extract table
    clean_names() %>% # clean column names
    mutate(week = parse_number(.[[4]])) %>% # extract numbers from 4th column
    filter(!is.na(week)) %>% # remove nulls to clean data and filter to reg season only
    select(date = 1, opp = 2, score = 3, spread = 5, total = 6, week) %>% # select and rename columns
    mutate(team = team_slug,
           year = season,
           line = sub(".*? (.+)", "\\1", spread),
           o_u = sub(".*? (.+)", "\\1", total)) %>% 
    select(team, year, week, date, opp, score, line, o_u)
}

# set a delay to scrape url responsibly
rate <- rate_delay(pause = 3)

slow_vegas_scrape <- slowly(scrape_vegas, rate = rate)

# run scrape jobs across all cores
plan(multisession, workers = availableCores())

tic()
vegas_logs <- future_map_dfr(url_list, slow_vegas_scrape)
toc()