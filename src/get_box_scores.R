# filters
positions <- c('qb','wr','rb','te')
weeks <- c(1:16)
years <- c(2014:2021)

# get box scores
box_scores <- load_player_stats(seasons = TRUE) %>% 
  filter(season %in% years & week %in% weeks)