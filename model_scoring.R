#package management
pacman::p_load(tidyverse, yardstick)

df <- read_csv("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\data\\rewind\\week15.csv") %>%
  rename(model = `My Proj`,
         cruncher = `FC Proj`,
         actual = `Actual Score`) %>%
  mutate(cruncher_error = abs(actual - cruncher),
         model_error = abs(actual - model)) %>%
  select(Player, Pos, Salary, cruncher, model, actual, cruncher_error, model_error) %>%
  filter(cruncher > 0.01 & model > 0.01 & actual > 0.01 & cruncher != model)

cat("Fantasy Cruncher's overall MAE is",round(df %>% mae(actual, cruncher) %>% .$.estimate,1), 
    "\nOur Model's overall MAE is",round(df %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's QB MAE is",round(df %>% filter(Pos == 'QB') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's QB MAE is",round(df %>% filter(Pos == 'QB') %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's RB MAE is",round(df %>% filter(Pos == 'RB') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's RB MAE is",round(df %>% filter(Pos == 'RB') %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's WR MAE is",round(df %>% filter(Pos == 'WR') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's WR MAE is",round(df %>% filter(Pos == 'WR') %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's TE MAE is",round(df %>% filter(Pos == 'TE') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's TE MAE is",round(df %>% filter(Pos == 'TE') %>% mae(actual, model) %>% .$.estimate,1)
    )

# read individual files and perform
file_list <- list.files("C:\\Users\\JakeWaddle\\Desktop\\nfl_dfs\\data\\rewind\\", pattern = 'week', full.names = TRUE, recursive = TRUE)



process_fun <- function(file) {
  read_csv(file) %>%
    rename(model = `My Proj`,
           cruncher = `FC Proj`,
           actual = `Actual Score`) %>%
    mutate(cruncher_error = abs(actual - cruncher),
           model_error = abs(actual - model)) %>%
    select(Player, Pos, Salary, cruncher, model, actual, cruncher_error, model_error) %>%
    filter(cruncher > 0.01 & model > 0.01 & actual > 0.01 & cruncher != model)
}

df <- file_list %>% map_df(., process_fun)

cat("Fantasy Cruncher's overall MAE is",round(df %>% mae(actual, cruncher) %>% .$.estimate,1), 
    "\nOur Model's overall MAE is",round(df %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's QB MAE is",round(df %>% filter(Pos == 'QB') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's QB MAE is",round(df %>% filter(Pos == 'QB') %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's RB MAE is",round(df %>% filter(Pos == 'RB') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's RB MAE is",round(df %>% filter(Pos == 'RB') %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's WR MAE is",round(df %>% filter(Pos == 'WR') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's WR MAE is",round(df %>% filter(Pos == 'WR') %>% mae(actual, model) %>% .$.estimate,1),
    "\nFantasy Cruncher's TE MAE is",round(df %>% filter(Pos == 'TE') %>% mae(actual, cruncher) %>% .$.estimate,1),
    "\nOur Model's TE MAE is",round(df %>% filter(Pos == 'TE') %>% mae(actual, model) %>% .$.estimate,1)
)
