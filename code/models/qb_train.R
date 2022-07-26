# package management
library(pacman)
p_load(tidyverse, h2o, beepr, lime, vip, pdp)

# working path
path <- 'C:/Users/JakeWaddle/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE)

# select quarterback feature columns
qbTrain <- df %>%
  filter(pos == 'QB'& pass_att > 20) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars('opponent.completion.pct','opponent.red.zone.scoring.pct'),
            funs(as.numeric(gsub('%','', .)))) %>%
  select(tag,salary, h_a, is_dome, is_dome_favorite, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|[[:digit:]]')) %>%
  select(tag, salary, h_a, is_dome, is_dome_favorite, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|*pass_att|*pass_comp|*pass_yards|*pass_td|*int|*rush_att|*rush_yards|*rush_td|*air_yards|*fdp')) %>%
  select(-starts_with('rz_rush')) %>%
  mutate_at(vars('h_a', 'is_dome', 'is_dome_favorite', 'is_total_fifty', 'o_u.binned', 'vegas'),
            funs(factor)) %>%
  filter(!is.na(pass_yards_sum5)) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

# start up the h2o jvm
h2o.init(nthread = -1)

# h2o split and train
qb_train  <- as.h2o(qbTrain)

#Split data into Train/Validation/Test Sets
qb_split_h2o <- h2o.splitFrame(qb_train, c(0.6, 0.2), seed = 1234)
qb_train__h2o <- h2o.assign(qb_split_h2o[[1]], "train" ) # 60%
qb_valid__h2o <- h2o.assign(qb_split_h2o[[2]], "valid" ) # 20%
qb_test__h2o  <- h2o.assign(qb_split_h2o[[3]], "test" )  # 20%

# Set names for h2o
target <- 'tag'
qb_predictors <- setdiff(names(qb_train__h2o), target)

# train model
qb_automl_h2o_models <- h2o.automl(
  x = qb_predictors, 
  y = target,
  training_frame    = qb_train__h2o,
  leaderboard_frame = qb_valid__h2o,
  max_runtime_secs = 3600
)

# best model
qb_best_model <- qb_automl_h2o_models@leader

# model evaluation
vip(qb_best_model, num_features = 20)

explainer <- lime(qbTrain, qb_best_model)

explanation <- lime::explain(qbTrain[1:25,], 
                                    explainer, 
                                    n_features = 10)

plot_features(explanation, ncol = 4) + ggtitle("autoML")

# save model 
h2o.saveModel(object=qb_best_model, path=paste0(path, 'code/models/qb_model'), force=TRUE)

# shut down
h2o.shutdown(prompt = FALSE)


