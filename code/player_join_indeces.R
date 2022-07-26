#### load packages ####
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(fuzzyjoin, tidyverse)

### need to read in the data files ###
# read in data 
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\data\\'
filenames <- list.files(path = path, pattern="*.csv")
files <- lapply(list.files(path = path, pattern="*.csv"), function(x) paste(path, x, sep = ''))
list2env(
  lapply(setNames(files, make.names(gsub("*.csv$", "", filenames))), function(x)
         read.csv(x, stringsAsFactors = FALSE)), envir = .GlobalEnv)

rm(files)
rm(path)
rm(filenames)
rm(pbp_history)
######################################


# dfs players - left hand side of all joins
dfs_players <- dfs_history %>% filter(pos != 'Def') %>% mutate(player = tolower(gsub('\\.', '', player))) %>%
  mutate(player = ifelse(player == 'odell beckham jr', 'odell beckham',
                         ifelse( player == 'patrick mahomes ii', 'patrick mahomes', player))) %>% distinct(player)
dfs_wr_te <- dfs_history %>% filter(pos == 'WR' | pos == 'TE') %>% mutate(player = tolower(gsub('\\.', '', player))) %>%
  mutate(player = ifelse(player == 'odell beckham jr', 'odell beckham', player)) %>% distinct(player)
dfs_rb <- dfs_history %>% filter(pos == 'RB') %>% mutate(player = tolower(gsub('\\.', '', player))) %>% distinct(player)

# player stats players
player_stats_players <- player_stats_history %>% mutate(player = tolower(gsub('\\.', '',name))) %>%
  mutate(player = ifelse(player == 'odell beckham jr', 'odell beckham',
                         ifelse(player == 'joshua jacobs', 'josh jacobs', player))) %>% distinct(player)

# red zone receiving targets players
rz_rec_players <- receiving_targets_history %>%
  mutate(name = ifelse(name == 'trequan smith', "tre`quan smith", name),
         name = tolower(sub("(\\w+),\\s(\\w+)","\\2 \\1", gsub('\\.', '', name)))) %>%
  distinct(name)

# red zone rush attaempts players
rz_rush_players <- red_zone_rush_attempts_history %>%
  mutate(name = tolower(sub("(\\w+),\\s(\\w+)","\\2 \\1", gsub('\\.', '', name)))) %>%
  distinct(name)

# play by play players
air_yards_player <- air_yards %>%
  mutate(full_name = tolower(gsub('\\.', '', full_name))) %>%
  distinct(full_name) %>%
  filter(complete.cases(.))

# all player xref between dfs logs and player box scores
all_player_xref <- dfs_players %>%
  stringdist_left_join(player_stats_players, by = c(player = 'player'),
                        distance_col = "distance",
                        max_dist = 2,
                        method = 'lv') %>%
  group_by(player.x) %>%
  top_n(-1) %>%
  ungroup() %>%
  dplyr::rename(dfs_name = player.x, playerstats_name = player.y) %>%
  select(-distance) 

# wr, te xref between dfs logs and nfl savant
wr_te_xref <- dfs_wr_te %>%
  mutate(player = gsub('\\.','',player)) %>%
  stringdist_left_join(rz_rec_players, by = c(player = 'name'),
                       distance_col = "distance",
                       max_dist = 2,
                       method = 'lv') %>%
  group_by(player) %>%
  top_n(-1) %>%
  ungroup() %>%
  dplyr::rename(dfs_name = player, savant_name = name) %>%
  select(-distance)

# rb xref between dfs logs and nfl savant
rb_xref <- dfs_rb %>%
  mutate(player = gsub('\\.','',player)) %>%
  stringdist_left_join(rz_rush_players, by = c(player = 'name'),
                       distance_col = "distance",
                       max_dist = 2,
                       method = 'lv') %>%
  group_by(player) %>%
  top_n(-1) %>%
  ungroup() %>%
  dplyr::rename(dfs_name = player, savant_name = name) %>%
  select(-distance)

# union the savant xrefs together
savant_xref <- rbind(wr_te_xref, rb_xref)

# join savant xref to all player xref
player_xref <- all_player_xref %>%
  left_join(savant_xref, by = 'dfs_name') %>%
  distinct(dfs_name, playerstats_name, savant_name) %>%
  mutate(dfs_name_2 = gsub('\\.', '', dfs_name)) %>%
  stringdist_left_join(air_yards_player, by = c(dfs_name_2 = 'full_name'),
                       distance_col = "distance",
                       max_dist = 0,
                       method = 'lv') %>%
  dplyr::rename(air_yards_name = full_name) %>%
  select(-distance, -dfs_name_2)

# play by play index
pbp_players <- rushes_inside_10 %>%
  distinct(full_player_name) %>%
  mutate_all(tolower)

to_join <- player_xref %>% 
  mutate(first = sapply(strsplit(savant_name, ' '), function(x) x[1]),
         last = sapply(strsplit(savant_name, ' '), function(x) x[2]),
         f_initial = as.character(paste0(substr(first, 1,1), '.', last)))  %>%
  filter(f_initial != 'NA.NA' & !is.na(f_initial))

pbp <- pbp_players %>%
  filter(complete.cases(.)) %>%
  stringdist_left_join(to_join, by = c(full_player_name = "dfs_name"),
                       distance_col = "distance",
                       max_dist = 0,
                       method = 'lv') %>%
  group_by(full_player_name) %>%
  top_n(-1) %>%
  ungroup() %>%
  select(dfs_name, full_player_name) %>%
  dplyr::rename(pbp_name = full_player_name)

player_xref <- player_xref %>%
  left_join(pbp, by = 'dfs_name')

# save
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\data\\'
write.csv(player_xref, paste(path, 'player_xref.csv', sep = ''), row.names = FALSE)

