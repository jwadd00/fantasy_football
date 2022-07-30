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
         pacr = as.numeric(pass_yards / air_yards),
         hvt = as.numeric(rushes_inside_10 + rec),
         hvt_ratio = as.numeric(hvt / (rush_att + rec))) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  

# get lag values
lag_features <- df %>%
  group_by(player) %>%
  arrange(year, week) %>%
  mutate_at(vars(c(feature_columns, 'adot', 'pacr','hvt','hvt_ratio')),
            funs(lag)) %>%
  ungroup()

# define window widths
window_widths <-seq(3, 5, by = 2)

# define aggregations
aggs <- c('sum', 'median', 'max', 'min')

# cartesian product of widths and aggregates
aggregate_combinations <- expand.grid(f = aggs, w = window_widths)

# function to create aggregate functions
generateFunctions <- function(f,w) {
  aggregate_strings <- gsub(' ', '',paste(f,w,' <- ','rollify(',f,', ','window =',w,')', sep='')) # paste together strings to create the rollify functions
  }

# apply generateFunctions function to combinations of widths and aggregates - this creates a list of funcitons
aggregate_functions <- apply(aggregate_combinations, 1, function(x) do.call(generateFunctions, as.list(x)))

# can evaluate strings with - this stores the functions we just created
eval(parse(text=aggregate_functions))

# reference functions as a list
function_list <- gsub("<.*","",aggregate_functions)

# run all functions on features
features_train <- lag_features %>%
  distinct(player, year, week, .keep_all = TRUE) %>%
  group_by(player) %>%
  mutate(len = n()) %>%
  ungroup() %>%
  filter(len >= 5) %>%
  group_by(player) %>%
  arrange(year, week) %>%
  mutate_at(vars(c(feature_columns, 'adot', 'pacr', 'hvt', 'hvt_ratio')),
            funs_(function_list)) %>%
  ungroup()

# in-line features
#features_train <- features_train %>%
#  mutate(vegas = ifelse(line > 0 & h_a == 'h', 'home_underdog',
#                        ifelse(line > 0 & h_a == 'a', 'road_underdog',
#                               ifelse(line <= 0 & h_a == 'h', 'home_favorite',
#                                      ifelse(line <= 0 & h_a == 'a', 'road favorite', 'unknown')))),
#         team_total = ifelse(line <= 0, (o_u / 2) + (abs(line)/2),
#                                   ifelse(line > 0, (o_u / 2) - (abs(line)/2),
#                                   o_u / 2)),
#         o_u.binned = cut(o_u, breaks = c(-Inf,30,35,40,45,50,55, Inf)),
#         is_dome = ifelse(weather == 'DOME', 1, 0),
#         is_dome_favorite = ifelse(is_dome == 1 & line <= 0, 1, 0),
#         is_total_fifty = ifelse(o_u > 50,1,0))


# save
write_csv(features_train, paste0(path, 'data/master/trainingData.csv'))

















