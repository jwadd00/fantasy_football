#### load packages ####
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(fuzzyjoin, tidyverse, tidyquant)

# read in data 
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\data\\'
filenames <- list.files(path = path, pattern="*.csv")
files <- lapply(list.files(path = path, pattern="*.csv"), function(x) paste(path, x, sep = ''))
list2env(
  lapply(setNames(files, make.names(gsub("*.csv$", "", filenames))), function(x)  
         read.csv(x, stringsAsFactors=F)), envir = .GlobalEnv)

# separate team stats into team stats and opponent stats
team_stats <- nfl_team_stats_history %>%
  filter(average.scoring.margin != '--') %>%
  select(-starts_with('opp'), -sacks.per.game)

opp_stats <- nfl_team_stats_history %>%
  select(Team, year, week, starts_with('opp'), sacks.per.game) %>%
  dplyr::rename(opp = Team)

# join
dfs_master <- dfs_history %>%
  mutate(player = tolower(gsub('\\.', '', player))) %>%
  mutate(player = ifelse(player == 'odell beckham jr', 'odell beckham',
                         ifelse(player == 'patrick mahomes ii', 'patrick mahomes', player))) %>%
  left_join(teams_xref, by = c(team = 'dfs_team')) %>%
  left_join(teams_xref, by = c(opp = 'dfs_team')) %>%
  select(-team_stats_team.y, -weather_team.y, -vegas_team.y) %>%
  dplyr::rename(city = city.x, team_stats_team = team_stats_team.x, weather_team = weather_team.x, vegas_team = vegas_team.x, opp_city = city.y) %>%
  left_join(vegas_history, by = c(vegas_team = 'team', year = 'year', week = 'week')) %>%
  left_join(weather_history, by = c(weather_team = 'team', year = 'year', week = 'week')) %>%
  left_join(team_stats, by = c(city = 'Team', year = 'year', week = 'week')) %>%
  left_join(opp_stats, by = c(opp_city = 'opp', year = 'year', week = 'week')) %>%
  dplyr::rename(opp = opp.x) %>% select(-opp.y) %>%
  mutate(player = tolower(player)) %>%
  left_join(player_xref, by = c(player = 'dfs_name')) %>%
  left_join(player_stats_history %>% mutate(name = ifelse(name == 'Odell Beckham Jr.', 'odell beckham', tolower(gsub('\\.', '', name)))), by = c(playerstats_name = 'name', year = 'year', week = 'week')) %>%
  dplyr::rename(pos = pos.x, player = player.x) %>% select(-pos.y, -player.y) %>%
  left_join(receiving_targets_history %>% mutate(name = tolower(sub("(\\w+),\\s(\\w+)","\\2 \\1", gsub('\\.', '', name)))) %>% select(name, targets, week, year), by = c(savant_name = 'name', year = 'year', week = 'week')) %>%
  left_join(red_zone_receiving_targets_history %>% mutate(name = tolower(sub("(\\w+),\\s(\\w+)","\\2 \\1", gsub('\\.', '', name)))) %>% dplyr::rename(rz_rec_td = td) %>% select(name, rz_targets, rz_rec_td, week, year), by = c(savant_name = 'name', year = 'year', week = 'week')) %>%
  left_join(red_zone_rush_attempts_history %>% mutate(name = tolower(sub("(\\w+),\\s(\\w+)","\\2 \\1", gsub('\\.', '', name)))) %>% dplyr::rename(rz_rush_td = rz_td) %>% select(name, rz_rush_att, rz_rush_td, week, year), by = c(savant_name = 'name', year = 'year', week = 'week')) %>%
  left_join(air_yards %>% mutate(full_name = tolower(gsub('\\.', '', full_name))), by = c(air_yards_name = 'full_name', year = 'season', week = 'week')) %>%
  left_join(rushes_inside_10 %>% mutate(rusher = tolower(full_player_name)) %>% select(-full_player_name, -posteam), by = c(pbp_name = 'rusher', year = 'season', week = 'week')) %>%
  distinct(player, year, week, .keep_all = TRUE)
  
# write
write.csv(dfs_master, paste(path, 'master\\dfs_history.csv', sep = ''), row.names = FALSE) 
