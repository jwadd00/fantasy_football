# package management
library(pacman)
p_load(parsnip, tidymodels, caret, xgboost)

# working path
path <- 'C:/Users/JakeWaddle/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE)

# select quartewrack feature columns
wrTrain <- df %>%
  filter(pos == 'WR') %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars('opponent.completion.pct','opponent.red.zone.scoring.pct'),
            funs(as.numeric(gsub('%','', .)))) %>%
  select(tag, salary, h_a, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|[[:digit:]]')) %>%
  select(tag, salary, h_a, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|*team_air|*air_yards|*rz_rec|*target|rec|*fdp|*aypt|*adot|*racr|*target_share|*wopr')) %>%
  select(tag, salary, h_a, rec_yards_sum5, targets_sum5, target_share_sum5, target_share_sum3, fdp_sum5, rec_yards_min5, wopr_max5, rec_sum5, air_yards_median5, fdp_min5, wopr_sum3, first.downs.per.play, fdp_sum3,
         fdp_max5, team_air_median5, wopr_median5, targets_min3) %>%
  select(-starts_with('rz_rush')) %>%
  mutate_at(vars('h_a'),
            funs(factor)) %>%
  filter(!is.na(rec_yards_sum5)) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

# inspect for nulls
#inspectdf::inspect_num(wrTrain %>% select(salary, tag, fdp_median3)) %>% inspectdf::show_plot()

# train and test split
set.seed(10)
train_test_split <- initial_split(wrTrain, prop = 0.8)

wr_train <- training(train_test_split)
wr_test <- testing(train_test_split) 

# pre processing
wr_recipe <- recipe(tag ~ ., data = wr_train) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes())  %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep(training = wr_train)

train <- juice(wr_recipe) %>%  select_if(~ !any(is.na(.)))
test <- bake(wr_recipe, wr_test) %>%  select_if(~ !any(is.na(.)))

# train model

# cross validation
grid <- expand.grid(
  nrounds = c(200),
  eta = c(0.025),
  max_depth = c(2),
  gamma = 0,
  colsample_bytree =  c(0.4),
  min_child_weight = c(1),
  subsample = c(0.75)
)


train_control <- trainControl(method="repeatedcv", number = 5, repeats = 5, verboseIter = T, allowParallel = TRUE)
#elastic_train_control <- trainControl(method = 'repeatedcv', number = 5, repeats = 5, search = "random", verboseIter = T)

#spline_model <- train(tag~., data=train, method="earth", tuneGrid = data.frame(degree = 2, nprune = 5), trControl = trainControl(method = "cv", number = 5))
xgb_model <- train(tag~., data=train, method="xgbTree", trControl=train_control,tuneGrid=grid)
#elastic_net_model <- train(tag~., data=train, method="glmnet", tuneLength = 10, trControl = elastic_train_control)
#rf_model <- train(tag~., data = train, method = 'ranger', trControl = elastic_train_control)

varImp(xgb_model)

# eval model

# predict
#spline_predictions <- predict(spline_model, test)
xgb_predictions <- predict(xgb_model, test)
#elastic_net_predictions <- predict(elastic_net_model, test)
#rf_predictions <- predict(rf_model, test)

wr_test <- wr_test %>%
  mutate(xgb_fit = as.numeric(xgb_predictions),
         resid = abs(tag - xgb_fit)) %>%
  select(xgb_fit, tag, everything())


cat("The XGBOOST Model MAE is ", wr_test %>% mae(tag, xgb_fit) %>% .$.estimate)

saveRDS(xgb_model, paste0(path,"code/models/wr_model.rds"))
saveRDS(wr_recipe, paste0(path,"code/models/wr_recipe.rds"))

