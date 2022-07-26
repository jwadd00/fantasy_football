### run data acquisition code
source("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\data_acquisition.R")

rm(list = ls(all.names = TRUE))

### join all data together
source("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\data_joins.R")

rm(list = ls(all.names = TRUE))

### calculate model features for train
source("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\features_train.R")

rm(list = ls(all.names = TRUE))

### calculate model features for live predictions
source("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\features_live.R")

rm(list = ls(all.names = TRUE))

### train and score qb model
source('C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\models\\qb_train_2.R')

rm(list = ls(all.names = TRUE))

### train and score rb model
source('C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\models\\rb_train_2.R')

rm(list = ls(all.names = TRUE))

### train and score wr model
source('C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\models\\wr_train_2.R')

rm(list = ls(all.names = TRUE))

### train and score te model
source('C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\models\\te_train_2.R')

rm(list = ls(all.names = TRUE))

### get predictions for current week
source("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\code\\run_live.R")
