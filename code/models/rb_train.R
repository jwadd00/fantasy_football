# package management
library(pacman)
p_load(tidyverse, h2o, beepr, lime, vip, pdp)

# working path
path <- 'C:/Users/jwaddle/OneDrive - Chesapeake Energy/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE)

# select quarterback feature columns
rbTrain <- df %>%
  filter(pos == 'RB' & rush_att > 5) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars('opponent.completion.pct','opponent.red.zone.scoring.pct'),
            funs(as.numeric(gsub('%','', .)))) %>%
  select(tag, salary, h_a, is_dome, is_dome_favorite, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|[[:digit:]]')) %>%
  select(tag, salary, h_a, is_dome, is_dome_favorite, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|*rz_|*targe*|*team_air|*rush_att|*rush_yards|*rush_td|*air_yards|*fdp')) %>%
  mutate_at(vars('h_a', 'is_dome', 'is_dome_favorite', 'is_total_fifty', 'o_u.binned', 'vegas'),
            funs(factor)) %>%
  filter(!is.na(rush_yards_sum5)) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

# start up the h2o jvm
h2o.init(nthread = -1)

# h2o split and train
rb_train  <- as.h2o(rbTrain)

#Split data into Train/Validation/Test Sets
rb_split_h2o <- h2o.splitFrame(rb_train, c(0.6, 0.2), seed = 3234 )
rb_train__h2o <- h2o.assign(rb_split_h2o[[1]], "train" ) # 60%
rb_valid__h2o <- h2o.assign(rb_split_h2o[[2]], "valid" ) # 20%
rb_test__h2o  <- h2o.assign(rb_split_h2o[[3]], "test" )  # 20%

# Set names for h2o
target <- 'tag'
rb_predictors <- setdiff(names(rb_train__h2o), target)

# train model
rb_automl_h2o_models <- h2o.automl(
  x = rb_predictors, 
  y = target,
  training_frame    = rb_train__h2o,
  leaderboard_frame = rb_valid__h2o,
  max_runtime_secs = 300
)

# best model
rb_best_model <- rb_automl_h2o_models@leader

# model evaluation
vip(rb_best_model, num_features = 20)

explainer <- lime(rbTrain, rb_best_model)

explanation <- lime::explain(rbTrain[800:803,], 
                             explainer, 
                             n_features = 7)

plot_features(explanation, ncol = 4) + ggtitle("autoML")

# save model 
h2o.saveModel(object=rb_best_model, path=paste0(path, 'code/models/rb_model'), force=TRUE)

# shut down
h2o.shutdown(prompt = FALSE)
