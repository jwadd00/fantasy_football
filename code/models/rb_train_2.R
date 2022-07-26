# package management
library(pacman)
p_load(parsnip, tidymodels, caret, earth, glmnet, xgboost)

# working path
path <- 'C:/Users/JakeWaddle/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE)

# select quarterback feature columns
rbTrain <- df %>%
  filter(pos == 'RB' & rush_att + rec > 5) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars('opponent.completion.pct','opponent.red.zone.scoring.pct'),
            list( ~ as.numeric(gsub('%','', .)))) %>%
  select(tag, salary, h_a, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|[[:digit:]]')) %>%
  select(tag, salary, h_a, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|*rz_|*targe*|*team_air|*rush_att|*rush_yards|*rush_td|*air_yards|*fdp|*aypt|*adot|*racr|*target_share|*wopr|*hvt')) %>%
  mutate_at(vars('h_a'),
            list(~ as.factor(.))) %>%
  filter(!is.na(rush_yards_sum5)) %>%
  mutate_if(is.numeric,
            list(~ replace(., is.na(.), 0))) %>%
  select(-rz_rec_td_min5)

# inspect for nulls
inspectdf::inspect_num(rbTrain %>% select(salary, tag, fdp_median3)) %>% inspectdf::show_plot()

# train and test split
set.seed(13)
train_test_split <- initial_split(rbTrain, prop = 0.8)

rb_train <- training(train_test_split)
rb_test <- testing(train_test_split)

# pre processing
rb_recipe <- recipe(tag ~ ., data = rb_train) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes())  %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep(training = rb_train)

train <- juice(rb_recipe) %>%  select_if(~ !any(is.na(.)))
test <- bake(rb_recipe, rb_test) %>% select_if(~ !any(is.na(.)))

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
#varImp(spline_model)
#varImp(elastic_net_model)
#varImp(rf_model)

# eval model

# predict
#spline_predictions <- predict(spline_model, test)
xgb_predictions <- predict(xgb_model, test)
#elastic_net_predictions <- predict(elastic_net_model, test)
#rf_predictions <- predict(rf_model, test)

rb_test <- rb_test %>%
  mutate(xgb_fit = as.numeric(xgb_predictions),
         resid = abs(tag - xgb_fit)) %>%
  select(xgb_fit, tag, everything())

cat("The XGBOOST Model MAE is ", rb_test %>% mae(tag, xgb_fit) %>% .$.estimate)

saveRDS(xgb_model, paste0(path,"code/models/rb_model.rds"))
saveRDS(rb_recipe, paste0(path,"code/models/rb_recipe.rds"))