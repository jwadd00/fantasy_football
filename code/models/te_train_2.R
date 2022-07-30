# package management
library(pacman)
p_load(parsnip, tidymodels, caret, xgboost)

# working path
path <- 'C:/Users/JakeWaddle/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE)

# select quarterback feature columns
teTrain <- df %>%
  filter(pos == 'TE') %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(vars('opponent.completion.pct','opponent.red.zone.scoring.pct'),
            list(~ as.numeric(gsub('%','', .)))) %>%
  select(tag, salary, h_a, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|[[:digit:]]')) %>%
  select(tag, salary, h_a, average.scoring.margin, first.downs.per.play, plays.per.game, points.per.game, touchdowns.per.game,
         matches('*opponent|*team_air|*air_yards|*rz_rec|*target|rec|*fdp|*aypt|*adot|*racr|*target_share|*wopr')) %>%
  select(-starts_with('rz_rush')) %>%
  mutate_at(vars('h_a'),
            list(~ as.factor(.))) %>%
  filter(!is.na(rec_yards_sum5)) %>%
  mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

# train and test split
set.seed(13)
train_test_split <- initial_split(teTrain, prop = 0.8)

te_train <- training(train_test_split)
te_test <- testing(train_test_split)

# pre processing
te_recipe <- recipe(tag ~ ., data = te_train) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes())  %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep(training = te_train)

train <- juice(te_recipe) %>%  select_if(~ !any(is.na(.)))
test <- bake(te_recipe, te_test) %>% select_if(~ !any(is.na(.)))

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

te_test <- te_test %>%
  mutate(xgb_fit = as.numeric(xgb_predictions),
         resid = abs(tag - xgb_fit)) %>%
  select(xgb_fit, tag, everything())

ggplot(te_test %>% filter(salary > 4500), aes(x = salary, y = tag)) + 
  geom_segment(aes(xend = salary, yend = xgb_fit), alpha = 0.2) + 
  geom_point(aes(color = abs(resid), size = abs(resid))) + 
  scale_color_continuous(low = 'black', high = 'red') +
  guides(color = FALSE, size = FALSE) + 
  geom_point(aes(y = xgb_fit), shape = 1) + 
  theme_bw()


cat("The XGBOOST Model MAE is ", te_test %>% mae(tag, xgb_fit) %>% .$.estimate)

saveRDS(xgb_model, paste0(path,"code/models/te_model.rds"))
saveRDS(te_recipe, paste0(path,"code/models/te_recipe.rds"))
