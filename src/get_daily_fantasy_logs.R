# generate cartesian product of weeks and years (seasons)
weeks <- c(1:16)
years <- c(2014:2021)
weeks_and_years <- crossing(week = weeks, year = years)

# base url
url1 <- Sys.getenv("FANTASY_FOOTBALL_DFS_URL")
url2 <- "&year="
url3 <- "&game=dk&scsv=1"

# function to generate URLs
generateURL <- function(week, year) {
  
  full_url <- paste(url1, week, url2, year, url3, sep = '')
}

# generate all URLs to scrape
url_list <- pmap(weeks_and_years, generateURL) %>% 
  unlist()

# scrape fucntion
scrape_dfs <- function(url) {
  
  df <- read_html(url) %>% # read html
    html_nodes('pre') %>% # select section
    html_text() %>% # remove tags and return content as text
    str_split(pattern = '\n') %>% # split line breaks into separate list elements
    unlist() %>% # unlist into character vector
    .[. != ""] %>% # remove empty elements
    tibble() %>% # convert to tibble of one unnamed column
    separate(col = '.', into = c('week', 'year','gid','player','pos','team','h_a','opp','dkp','salary'), sep = ';') %>% # split into columns and rows
    .[-1,] # remove header row
} 

# set a delay to scrape url responsibly
rate <- rate_delay(pause = 3)

slow_dfs_scrape <- slowly(scrape_dfs, rate = rate)

# run scrape jobs across all cores
plan(multisession, workers = availableCores())

tic()
dfs_logs <- future_map_dfr(url_list, slow_dfs_scrape)
toc()