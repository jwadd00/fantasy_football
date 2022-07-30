# package management
library(pacman)
p_load(tidyverse, h2o, beepr, lime, vip, pdp)

# working path
path <- 'C:/Users/jwaddle/OneDrive - Chesapeake Energy/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE)

# select quarterback feature columns
wrTrain <- df %>%
  filter(pos == 'WR' & targets > 1) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars('opponent.completion.pct','opponent.red.zone.scoring.pct'),
            funs(as.numeric(gsub('%','', .)))) %>%
  select(tag, salary, h_a, is_dome, is_dome_favorite, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|[[:digit:]]')) %>%
  select(tag, salary, h_a, is_dome, is_dome_favorite, team_total, is_total_fifty, o_u.binned, vegas, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|*team_air|*air_yards|*rz_rec|*target|rec|*fdp')) %>%
  select(-starts_with('rz_rush')) %>%
  mutate_at(vars('h_a', 'is_dome', 'is_dome_favorite', 'is_total_fifty', 'o_u.binned', 'vegas'),
            funs(factor)) %>%
  filter(!is.na(rec_yards_sum5)) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

# start up the h2o jvm
h2o.init(nthread = -1)

# h2o split and train
wr_train  <- as.h2o(wrTrain)

#Split data into Train/Validation/Test Sets
wr_split_h2o <- h2o.splitFrame(wr_train, c(0.6, 0.2), seed = 1198 )
wr_train__h2o <- h2o.assign(wr_split_h2o[[1]], "wr_train" ) # 60%
wr_valid__h2o <- h2o.assign(wr_split_h2o[[2]], "wr_valid" ) # 20%
wr_test__h2o  <- h2o.assign(wr_split_h2o[[3]], "wr_test" )  # 20%

# Set names for h2o
target <- 'tag'
wr_predictors <- setdiff(names(wr_train__h2o), target)

# train model
wr_auto_ml <- h2o.automl(
  x = wr_predictors, 
  y = target,
  training_frame    = wr_train__h2o,
  leaderboard_frame = wr_valid__h2o,
  max_runtime_secs = 300
)

# best model
wr_best_model <- wr_auto_ml@leader

# model evaluation
vip(wr_best_model, num_features = 20)

explainer <- lime(wrTrain, wr_best_model)

explanation <- lime::explain(wrTrain[800:803,], 
                             explainer, 
                             n_features = 7)

plot_features(explanation, ncol = 4) + ggtitle("autoML")

# save model 
h2o.saveModel(object=wr_best_model, path=paste0(path, 'code/models/wr_model'), force=TRUE)

# shut down
h2o.shutdown(prompt = FALSE)
