# package management
library(pacman)
p_load(tidyverse)

# working path
path <- 'C:/Users/JakeWaddle/Desktop/nfl_dfs/'

# load data
df <- read.csv(paste0(path, '/data/master/trainingData.csv'), stringsAsFactors = FALSE) %>%
  filter(pos == "QB") %>%
  mutate(n_tiles = ntile(opponent.plays.per.game, 5))

df %>% 
  group_by(n_tiles) %>%
  tally()

df %>% 
  filter(fdp > 5 & !is.na(fdp)) %>%
  group_by(n_tiles) %>%
  summarize(avg = median(fdp),
            left = min(opponent.plays.per.game),
            right = max(opponent.plays.per.game))

# opponent avg passer rating
df %>%
  filter(opponent.plays.per.game > 45) %>%
  ggplot(aes(x=opponent.plays.per.game)) + geom_density()

# fdp
df %>%
  filter(fdp > 5 & !is.na(fdp)) %>% 
  ggplot(aes(x=fdp, color = factor(ifelse(opponent.plays.per.game > 67 & opponent.average.team.passer.rating > 85,1,0)))) + geom_density() + 
  #ggplot(aes(x=fdp, color = factor(ntile(opponent.plays.per.game, 5)))) + geom_density() + 
  ylab('fdp density') + 
  ggtitle('qb fdp density')
