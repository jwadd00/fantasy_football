#### load packages ####
# package management
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(rvest, lubridate, reshape, tidyverse, future.apply, stringi, jsonlite)

# path
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\'

# read functions 
lapply(list.files(path = 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\R', pattern="func*", full.names = TRUE), function(x) source(x))

# inputs for functions
positions <- c('qb','wr','rb','te')
teams <- c(1:32)
weeks <- c(1:16)
years <- c(2016:2021)

# list of stats to get from teamrankings.com
stats <- c('opponent-passing-yards-per-game','opponent-completion-pct','opponent-yards-per-pass-attempt',
           'opponent-passing-touchdowns-per-game','opponent-average-team-passer-rating', 'sacks-per-game',
           'opponent-rushing-yards-per-game','opponent-rushing-touchdowns-per-game','opponent-yards-per-rush-attempt',
           'opponent-yards-per-game','opponent-plays-per-game','opponent-points-per-game','opponent-touchdowns-per-game',
           'opponent-red-zone-scoring-pct','plays-per-game','points-per-game','touchdowns-per-game','average-scoring-margin',
           'first-downs-per-play')


# data acquisition functions
nflDFS(weeks = weeks, years = years)
nflWeather(weeks = weeks, years = years)
nflReceivingTargets(weeks = weeks, years = years)
nflRedZoneTargets(weeks = weeks, years = years)
nflRedZoneRushAtt(weeks = weeks, years = years)
nflPlayerStats(positions = positions, years = years, weeks = weeks)
#nflVegas(teams = teams, years = years)
nflTeamStats(stats)
#nflAirYards(years)

# grab by play by play data
#source("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\scrapers\\adv_stats_pbp.R")

# update the path again
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\'

# save
write.csv(dfs_logs_nfl, paste(path, 'data\\dfs_history.csv', sep = ''), row.names = FALSE)
write.csv(dfs_weather_nfl, paste(path, 'data\\weather_history.csv', sep = ''), row.names = FALSE)
write.csv(nfl_team_stats_transform, paste(path, 'data\\nfl_team_stats_history.csv', sep = ''), row.names = FALSE)
#write.csv(nfl_vegas_history, paste(path, 'data\\vegas_history.csv', sep = ''), row.names = FALSE)
write.csv(player_stats_combined, paste(path, 'data\\player_stats_history.csv', sep = ''), row.names = FALSE)
write.csv(red_zone_receiving_targets, paste(path, 'data\\red_zone_receiving_targets_history.csv', sep = ''), row.names = FALSE)
write.csv(red_zone_rush_attempts, paste(path, 'data\\red_zone_rush_attempts_history.csv', sep = ''), row.names = FALSE)
write.csv(total_receiving_targets, paste(path, 'data\\receiving_targets_history.csv', sep = ''), row.names = FALSE)
#write.csv(air_yards, paste(path, 'data\\air_yards.csv', sep = ''), row.names = FALSE)
