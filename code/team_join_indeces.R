#### load packages ####
#devtools::install_github('MySportsFeeds/mysportsfeeds-r')
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(rvest, future.apply, tidyquant, fuzzyjoin, tidyverse, mysportsfeedsR)

### need to read in the historical data files ###
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\data\\'
filenames <- list.files(path = path, pattern="*.csv")
files <- lapply(list.files(path = path, pattern="*.csv"), function(x) paste(path, x, sep = ''))
list2env(
  lapply(setNames(files, make.names(gsub("*.csv$", "", filenames))), 
         read.csv), envir = .GlobalEnv)
######################################

# mysportsfeeds login to get team data
.MySportsFeedsEnv <- new.env()
.MySportsFeedsEnv$data <- list( v1_0_username <- "jwadd00", v1_0_password <- "J88aguar")
authenticate_v1_x('jwadd00', 'J88aguar')

# retrieve sample schedule to get the team data
schedule <- msf_get_results(version='1.2',league='nfl',season='2019-2020-regular',feed='full_game_schedule',params=list(format='csv'))[['api_json']]$fullgameschedule$gameentry

# msf team data
msf_team_data <- schedule %>%
  distinct(homeTeam.Abbreviation, homeTeam.Name, homeTeam.City) %>%
  dplyr::rename(msf_abbrev = homeTeam.Abbreviation,
                msf_nickname = homeTeam.Name,
                msf_team = homeTeam.City) %>%
  mutate(msf_abbrev = ifelse(trimws(tolower(msf_abbrev)) == 'la', 'lar', trimws(tolower(msf_abbrev))),
         msf_nickname = trimws(tolower(msf_nickname)),
         msf_team = trimws(tolower(msf_team)))

rm(schedule)

# dfs teams
dfs_teams <- dfs_history %>% distinct(team)
rm(dfs_history)

# vegas teams        
vegas_teams <- vegas_history %>% distinct(team)
rm(vegas_history)

# weather teams
weather_teams <- weather_history %>% distinct(team)
rm(weather_history)

# team stats teams
team_stats_teams <- nfl_team_stats_history %>% distinct(Team)
rm(nfl_team_stats_history)

# team xref
team_indeces <- dfs_teams %>%
  stringdist_inner_join(vegas_teams, by = c(team = 'team'),
                        distance_col = "distance",
                        method = 'lv') %>%
  group_by(team.x) %>%
  top_n(-1) %>%
  ungroup() %>%
  dplyr::rename(dfs_team = team.x, 
                vegas_team = team.y) %>%
  select(-distance) %>%
  stringdist_inner_join(msf_team_data, by = c(dfs_team = 'msf_abbrev'),
                        distance_col = "msf_dist",
                        method = 'lv') %>%
  group_by(msf_nickname) %>%
  top_n(-1, msf_dist) %>%
  ungroup() %>%
  select(-msf_dist, -msf_abbrev) %>%
  dplyr::rename(nickname = msf_nickname, city = msf_team) %>%
  stringdist_inner_join(weather_teams, by = c(nickname = 'team'),
                        distance_col = "dist",
                        method = 'lv') %>%
  group_by(dfs_team) %>%
  top_n(-1, dist) %>%
  ungroup() %>%
  dplyr::rename(weather_team = team) %>%
  mutate(city = ifelse(dfs_team == 'lac', 'la chargers',
                               ifelse(dfs_team == 'lar', 'la rams',
                                      ifelse(dfs_teams == 'nyj', 'ny jets',
                                             ifelse(dfs_team == 'nyg', 'ny giants', city))))) %>%
  select(-dist, -nickname) %>%
  stringdist_inner_join(team_stats_teams, by = c(city = 'Team'),
                        distance_col = "dist",
                        method = 'lv') %>%
  group_by(city) %>%
  top_n(-1, dist) %>%
  ungroup() %>%
  dplyr::rename(team_stats_team = Team) %>%
  select(city, team_stats_team, weather_team, dfs_team, vegas_team)
  
# save
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\'
write.csv(team_indeces, paste(path, 'data\\teams_xref.csv', sep = ''), row.names = FALSE)
  
  
  
  
  
  
