# package handling
library(pacman)
p_load(tidyverse, tibbletime)

# working path
path <- 'C:/Users/JakeWaddle/Desktop/nfl_dfs/'

# read data
df <- read_csv(paste0(path,"data/master/dfs_history.csv"))

# define feature columns
feature_columns <- c('fdp', 'pass_att', 'pass_comp', 'pass_yards', 'pass_td', 'int', 'rush_att', 'rush_yards', 'rush_td',
                     'rec', 'rec_yards', 'rec_td', 'targets', 'rz_targets', 'rz_rec_td', 'rz_rush_att', 'rz_rush_td',
                     'yac', 'air_yards', 'team_air', 'aypt', 'racr', 'target_share', 'wopr')

# convert to numeric
df <- df %>%
  mutate(tag = fdp) %>%
  mutate_at(vars(feature_columns), 
            funs(as.numeric)) %>%
  mutate(adot = as.numeric(air_yards / targets),
         pacr = as.numeric(pass_yards / air_yards)) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

df %>% 
  select(tag, contains('air')) %>% glimpse()


# group by opponent
opp_air_yards <- df %>% 
  group_by(opp) %>%
  select(week, year, opp, team_air) %>%
  distinct(.) %>%
  ungroup() %>%
  group_by(opp, year, week) %>%
  filter(team_air == max(team_air)) %>%
  ungroup() %>%
  group_by(opp) %>%
  arrange(year, week, opp) %>%
  mutate(lag_team_air = lag(team_air)) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  group_by(opp, year) %>%
  mutate(ytd_team_air = cummean(lag_team_air)) %>%
  ungroup()
  
  
