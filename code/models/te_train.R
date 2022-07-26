# package management
library(pacman)
p_load(tidyverse, h2o, beepr, lime, vip, pdp)

# working path
path <- 'C:/Users/jwaddle/OneDrive - Chesapeake Energy/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE)

# select quarterback feature columns
teTrain <- df %>%
  filter(pos == 'TE' & fdp > 1) %>%
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
te_train  <- as.h2o(teTrain)

#Split data into Train/Validation/Test Sets
te_split_h2o <- h2o.splitFrame(te_train, c(0.6, 0.2), seed = 1198 )
te_train__h2o <- h2o.assign(te_split_h2o[[1]], "te_train" ) # 60%
te_valid__h2o <- h2o.assign(te_split_h2o[[2]], "te_valid" ) # 20%
te_test__h2o  <- h2o.assign(te_split_h2o[[3]], "te_test" )  # 20%

# Set names for h2o
target <- 'tag'
te_predictors <- setdiff(names(te_train__h2o), target)

# train model
te_auto_ml <- h2o.automl(
  x = te_predictors, 
  y = target,
  training_frame    = te_train__h2o,
  leaderboard_frame = te_valid__h2o,
  max_runtime_secs = 300
)

# best model
te_best_model <- te_auto_ml@leader

# model evaluation
vip(te_best_model, num_features = 20)

explainer <- lime(teTrain, te_best_model)

explanation <- lime::explain(teTrain[800:803,], 
                             explainer, 
                             n_features = 7)

plot_features(explanation, ncol = 4) + ggtitle("autoML")

# save model 
h2o.saveModel(object=te_best_model, path=paste0(path, 'code/models/te_model'), force=TRUE)

# shut down
h2o.shutdown(prompt = FALSE)
