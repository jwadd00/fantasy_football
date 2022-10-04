# partial url strings
url_one <- 'https://www.teamrankings.com/nfl/stat/'
url_two <- '?date='

# establish sequence of dates
twenty_fourteen_dates <- lapply(seq(as.Date('2014-09-03'), as.Date('2014-12-17'), by = "week"), toString)
twenty_fifteen_dates <- lapply(seq(as.Date('2015-09-09'), as.Date('2015-12-23'), by = "week"), toString)
twenty_sixteen_dates <- lapply(seq(as.Date('2016-09-07'), as.Date('2016-12-21'), by = "week"), toString)
twenty_seventeen_dates <- lapply(seq(as.Date('2017-09-06'), as.Date('2017-12-20'), by = "week"), toString)
twenty_eighteen_dates <- lapply(seq(as.Date('2018-09-05'), as.Date('2018-12-19'), by = "week"), toString)
twenty_nineteen_dates <- lapply(seq(as.Date('2019-09-04'), as.Date('2019-12-18'), by = "week"), toString)
twenty_twenty_dates <- lapply(seq(as.Date('2020-09-13'), as.Date('2020-12-27'), by = "week"), toString)
twenty_twenty_one_dates <- lapply(seq(as.Date('2021-09-09'), as.Date('2021-12-28'), by = "week"), toString)
twenty_twenty_two_dates <- lapply(seq(as.Date('2022-09-07'), as.Date('2022-12-28'), by = "week"), toString)

# combine date lists
dates <- c(twenty_fourteen_dates, twenty_fifteen_dates, twenty_sixteen_dates, twenty_seventeen_dates,
           twenty_eighteen_dates, twenty_nineteen_dates, twenty_twenty_dates, twenty_twenty_one_dates,
           twenty_twenty_two_dates)

# list of stats to get
stats <- c('opponent-passing-yards-per-game','opponent-completion-pct','opponent-yards-per-pass-attempt',
           'opponent-passing-touchdowns-per-game','opponent-average-team-passer-rating', 'sacks-per-game',
           'opponent-rushing-yards-per-game','opponent-rushing-touchdowns-per-game','opponent-yards-per-rush-attempt',
           'opponent-yards-per-game','opponent-plays-per-game','opponent-points-per-game','opponent-touchdowns-per-game',
           'opponent-red-zone-scoring-pct','plays-per-game','points-per-game','touchdowns-per-game','average-scoring-margin',
           'first-downs-per-play')


# cartesian product of dates
dates_and_stats <- crossing(d = dates, s = stats)

# function to generate URLs using cartesian product dates_and_stats
generateTeamStatsURLs <- function(s,d) {
  
  full_team_stats_url <- paste(url_one,s,url_two,d,sep="")

}

# use apply to pass cartesian product of stats and dates through a function expecting two arguments


full_team_stats_url_strings <- apply(dates_and_stats, 1, function(x) do.call(generateTeamStatsURLs, as.list(x)))