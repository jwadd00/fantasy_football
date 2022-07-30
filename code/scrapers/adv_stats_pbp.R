# package management
pacman::p_load(tidyverse, nflfastR)

seasons <- 2010:2020
pbp <- purrr::map_df(seasons, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.csv.gz")
  )
})

positions <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL_Positions/master/nfl_positions_2011_2020.csv"))

# rushes inside 10 by week 
rushes_inside_10 <- pbp %>%
  left_join(positions, by = c("rusher_id" = "player_id")) %>%
  mutate(posteam = tolower(posteam)) %>%
  filter(rush_attempt == 1 & play_type == 'run' & yardline_100 <= 10) %>%
  group_by(season, week, posteam, full_player_name) %>%
  mutate(rushes_inside_10 = n()) %>%
  ungroup() %>%
  distinct(season, week, posteam, full_player_name, rushes_inside_10)

write_csv(rushes_inside_10, 'C:/Users/JakeWaddle/Desktop/nfl_dfs/data/rushes_inside_10.csv')

