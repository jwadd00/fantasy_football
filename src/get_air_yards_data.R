# partial url strings
url_one <- 'http://api.airyards.com/'
url_two <- '/weeks'

years <- 2016

url <- glue(url_one, years, url_two)

df <- fromJSON(url)

# grab air yards data from air yards dot com
df_list <- future_lapply(years, function(x) {
  if(x != 2020) {
    fromJSON(paste0(url_one,x,url_two)) %>%
      mutate(season = x) %>%
      select(full_name, season, week, tar, yac, air_yards, aypt, racr, target_share, wopr, tm_att, team_air)
  }
})

air_yards <<- do.call(rbind, df_list)