# package management
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(reshape, rvest, RCurl, lubridate, tibbletime, tidyverse, future.apply, caret, fuzzyjoin, stringr, recipes, readxl)

# set working path
path <- 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\'

# load models
qb_model <- readRDS(paste0(path,"code/models/qb_model.rds"))
qb_recipe <- readRDS(paste0(path,"code/models/qb_recipe.rds"))
rb_model <- readRDS(paste0(path,"code/models/rb_model.rds"))
rb_recipe <- readRDS(paste0(path,"code/models/rb_recipe.rds"))
wr_model <- readRDS(paste0(path,"code/models/wr_model.rds"))
wr_recipe <- readRDS(paste0(path,"code/models/wr_recipe.rds"))
te_model <- readRDS(paste0(path,"code/models/te_model.rds"))
te_recipe <- readRDS(paste0(path,"code/models/te_recipe.rds"))

# set week
w <- 15

# load all functions
lapply(list.files(path = 'C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\R', pattern="func*", full.names = TRUE), function(x) source(x))

# load team join index
team_index <- read.csv(paste(path, 'data\\teams_xref.csv', sep = ''), stringsAsFactors = FALSE)

# read vegas from saved file
vegas_current <- read_excel(paste0(path, 'data\\vegas\\vegas2019_xl.xlsx')) %>%
  select(team, line, o_u, team_total) %>%
  mutate(team = tolower(team))

# retrieve weather
nflWeather(weeks = w, years = 2019)

# load team stats
teamStats <- read.csv(paste0(path, 'data\\nfl_team_stats_history.csv'), stringsAsFactors = FALSE) %>%
  arrange(year, week) %>%
  filter(year == 2019 & week == w)

# separate team stats into team stats and opponent stats
team_stats <- teamStats %>%
  select(-starts_with('opp'), -sacks.per.game)

opp_stats <- teamStats %>%
  select(Team, year, week, starts_with('opp'), sacks.per.game) %>%
  dplyr::rename(opp = Team)

# retrieve salaries
salaries <- read.csv(paste0(path, 'data\\salaries\\salaries2019.csv'), stringsAsFactors = FALSE) %>%
  mutate(name = tolower(paste(First.Name, Last.Name, sep = ' '))) %>%
  mutate(name = gsub('\\.','',name)) %>%
  mutate(name = gsub("\\ii$", "", name)) %>%
  mutate(name = gsub("\\ i$", "", name)) %>%
  mutate(name = gsub("\\ jr$", "", name)) %>%
  mutate(name = str_trim(name, side = 'right')) %>%
  mutate(name = ifelse(name == 'willie snead iv', 'willie snead',
                       ifelse(name == "tre'quan smith", 'trequan smith',
                              ifelse(name == 'will fuller v', 'will fuller',
                                     ifelse(name == 'ted ginn', 'ted ginn jr', name)))),
         home_team = gsub(".*@","",Game),
         h_a = ifelse(Team == home_team, 'h','a')) %>%
  select(name, Salary, Position, Team, Opponent, h_a) %>%
  dplyr::rename(salary = Salary, pos = Position, team = Team, opp = Opponent)

# read live features
liveFeatures <- read.csv(paste0(path, 'data\\master\\liveFeatures.csv'), stringsAsFactors = FALSE) %>%
  group_by(player) %>%
  arrange(year, week) %>%
  filter(row_number() == max(row_number())) %>%
  ungroup() %>%
  stringdist_left_join(dfs_weather_nfl, by = c(weather_team = 'team'),
                       distance_col = "distance",
                       max_dist = 1,
                       method = 'lv') %>%
  group_by(city) %>%
  top_n(-2) %>%
  ungroup() %>%
  select(-distance) %>%
  stringdist_left_join(vegas_current, by = c(vegas_team = 'team'),
                       distance_col = "distance",
                       max_dist = 1,
                       method = 'lv') %>%
  group_by(vegas_team) %>%
  top_n(-1) %>%
  ungroup() %>%
  select(-ends_with('.x'), -h_a, -opp, -fdp, -salary, -opp_city, -game_date, -game, -score, -team.y, -team, -average.scoring.margin,
         -first.downs.per.play, -plays.per.game, -points.per.game, -starts_with('opponent'), -sacks.per.game,
         -touchdowns.per.game, -distance) %>%
  dplyr::rename(year = year.y, week = week.y, weather = weather.y, temperature = temperature.y,
                wind = wind.y, line = line.y, o_u = o_u.y) %>%
  stringdist_left_join(salaries, by = c(player = 'name'),
                       distance_col = 'distance',
                       max_dist = 2, 
                       method = 'lv') %>%
  group_by(player) %>% 
  top_n(-1) %>%
  ungroup() %>%
  mutate(team = tolower(team),
         opp = tolower(opp)) %>%
  left_join(team_stats, by = c(team_stats_team = 'Team')) %>%
  left_join(team_index, by = c(opp = 'dfs_team')) %>%
  left_join(opp_stats, by = c(team_stats_team.y = 'opp')) %>%
  mutate(vegas = ifelse(line > 0 & h_a == 'h', 'home_underdog',
                        ifelse(line > 0 & h_a == 'a', 'road_underdog',
                               ifelse(line <= 0 & h_a == 'h', 'home_favorite',
                                      ifelse(line <= 0 & h_a == 'a', 'road favorite', 'unknown')))),
         o_u.binned = cut(o_u, breaks = c(-Inf,30,35,40,45,50,55, Inf)),
         is_dome = ifelse(weather == 'DOME', 1, 0),
         is_dome_favorite = ifelse(is_dome == 1 & line <= 0, 1, 0),
         is_total_fifty = ifelse(o_u > 50,1,0)) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars('opponent.completion.pct','opponent.red.zone.scoring.pct'),
            funs(as.numeric(gsub('%','', .)))) %>%
  select(player,pos.x,salary, h_a, is_dome, is_dome_favorite, line, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|[[:digit:]]')) %>%
  select(player,pos.x,salary, h_a, is_dome, is_dome_favorite, line, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|*pass_att|*pass_comp|*pass_yards|*pass_td|*int|*rush_att|*rush_yards|*rush_td|*air_yards|*rz_|*targe*|*team_air|*rush_att|*rush_yards|*rush_td|*air_yards|*rz_rec|*target|rec|*fdp|*pacr|*aypt|*racr|*wopr|*adot|*hvt')) %>%
  mutate_at(vars('h_a', 'is_dome', 'is_dome_favorite', 'is_total_fifty', 'o_u.binned', 'vegas'),
            funs(factor)) %>%
  mutate_at(vars('opponent.average.team.passer.rating', 'opponent.passing.touchdowns.per.game','opponent.passing.yards.per.game',
                 'opponent.plays.per.game','opponent.points.per.game','opponent.rushing.touchdowns.per.game','opponent.rushing.yards.per.game',
                 'opponent.touchdowns.per.game','opponent.yards.per.game','opponent.yards.per.pass.attempt','opponent.yards.per.rush.attempt',
                 'average.scoring.margin','first.downs.per.play','plays.per.game','points.per.game','touchdowns.per.game'),
            funs(as.numeric)) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
#### predictions ####
qb_data <- liveFeatures %>% filter(pos.x == 'QB' & !is.na(vegas))
qb_df <- liveFeatures %>% filter(pos.x == 'QB' & !is.na(vegas)) %>% select(player)

qb_predictions <- cbind(qb_df,as.data.frame(predict(qb_model, qb_data %>% bake(qb_recipe, .))))
names(qb_predictions) <- c('player','estimate')

rb_data <- liveFeatures %>% filter(pos.x == 'RB' & !is.na(vegas))
rb_df <- liveFeatures %>% filter(pos.x == 'RB' & !is.na(vegas)) %>% select(player)

rb_predictions <- cbind(rb_df,as.data.frame(predict(rb_model, rb_data %>% bake(rb_recipe, .))))
names(rb_predictions) <- c('player','estimate')

wr_data <- liveFeatures %>% filter(pos.x == 'WR' & !is.na(vegas))
wr_df <- liveFeatures %>% filter(pos.x == 'WR' & !is.na(vegas)) %>% select(player)

wr_predictions <- cbind(wr_df,as.data.frame(predict(wr_model, wr_data %>% bake(wr_recipe, .))))
names(wr_predictions) <- c('player','estimate')

te_data <- liveFeatures %>% filter(pos.x == 'TE' & !is.na(vegas))
te_df <- liveFeatures %>% filter(pos.x == 'TE' & !is.na(vegas)) %>% select(player)

te_predictions <- cbind(te_df,as.data.frame(predict(te_model, te_data %>% bake(te_recipe, .))))
names(te_predictions) <- c('player','estimate')
  
predictions <- rbind(qb_predictions, rb_predictions, wr_predictions, te_predictions)

# read in grinders file
crunchers <- read.csv(paste0(path, 'data\\crunchers_list\\crunchers.csv'), stringsAsFactors = FALSE) %>%
  mutate(name = tolower(Player),
         name = ifelse(Player == 'Odell Beckham Jr.', 'odell beckham', name)) %>%
  mutate(name = gsub('\\.','',name)) %>%
  mutate(name = gsub("\\ii$", "", name)) %>%
  mutate(name = gsub("\\ i$", "", name)) %>%
  mutate(name = gsub("\\ jr$", "", name),
         name = ifelse(name == 'ted ginn', 'ted ginn jr', name)) %>%
  stringdist_left_join(predictions, by = c(name = 'player'),
                       distance_col = 'distance',
                       max_dist = 2, 
                       method = 'lv') %>%
  group_by(player) %>%
  top_n(-1) %>%
  ungroup() %>% 
  mutate(My.Proj = ifelse(FC.Proj < 5 | abs(estimate - FC.Proj) > estimate*0.5, FC.Proj, estimate)) %>%
  select(-player, -estimate, -distance, -name)

# save predictions
write.csv(crunchers, paste0(path, paste0('data/predictions/predictions_week',w,'.csv')), row.names = FALSE)
