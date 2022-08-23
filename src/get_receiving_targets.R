# partial url strings
url_one <- "http://www.nflsavant.com/targets.php?week="
url_two <- "&ddlYear="
url_three <- "&rz=all&ddlTeam=&ddlPosition="

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

# function to rip targets data from nfl savant
scrape_targets <- function(url) {

rec_targets <- url %>% 
  read_html() %>%
  html_nodes("table") %>%
  html_table %>%
  as.data.frame(.) %>%
  mutate(week = sub('\\&ddlYe.*', '',sub('.*week=', '', url)),
         year = sub('\\&rz=a.*', '',sub('.*Year=', '', url))) %>% 
  clean_names() %>% 
  select(year, week, name, team, pos, targets, completions)
}

# set a delay to scrape url responsibly
rate <- rate_delay(pause = 3)

slow_targets_scrape <- slowly(scrape_targets, rate = rate)

# run scrape jobs across all cores
plan(multisession, workers = availableCores())

tic()
rec_target_logs <- future_map_dfr(url_list, slow_targets_scrape)
toc()