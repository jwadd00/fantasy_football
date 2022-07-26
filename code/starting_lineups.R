# load packages
library(mysportsfeedsR)
library(tidyverse)
library(future.apply)

# auth for msf api
.MySportsFeedsEnv <- new.env()
.MySportsFeedsEnv$data <- list( v1_0_username <- "jwadd00", v1_0_password <- "J88aguar")
authenticate_v1_x('jwadd00', 'J88aguar')

# list of seasons
seasons <- c('2014-2015-regular', '2015-2016-regular', '2016-2017-regular', '2017-2018-regular', '2018-2019-regular')

# function to get the schedule
getSchedule <- function(season) {

  schedule <- msf_get_results(version='1.2',league='nfl',season=season,feed='full_game_schedule',params=list(format='csv'))
  schedule <- schedule[['api_json']]$fullgameschedule$gameentry
  schedule <- schedule %>%
    mutate(season_id = season)
}

# get schedules
nfl_schedules <- do.call(rbind, lapply(seasons, function(x) getSchedule(x)))

# list of game id's
game_ids <- unique(nfl_schedules %>% select(season_id, id))

# function to get starting lineups
get_starting_lineups <- function(season_id, id) {
  
  # api calls
  lineups <- msf_get_results(version='1.2',league='nfl',season = season_id, feed='game_startinglineup',params=list(gameid=id,format='csv'))
  lineups <- lineups[['api_json']][['gamestartinglineup']]$teamLineup$actual.starter
  lineups <- map_df(lineups, rbind)
  lineups <- lineups %>%
    mutate(season_id = season_id, 
           game_id = id)
}

safely_get_starting_lineups <- safely(get_starting_lineups)

# get starting lineup data
starting_lineups <- apply(game_ids ,1, function(x) do.call(safely_get_starting_lineups, as.list(x)))

starting_lineups_result <- starting_lineups %>%
  map_df(~.x$result)

write.csv(starting_lineups_result, './data/staring_lineup_history.csv', row.names = FALSE)
