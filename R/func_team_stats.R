nflTeamStats <- function(stats) {
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

	# combine date lists
	dates <- c(twenty_fourteen_dates, twenty_fifteen_dates, twenty_sixteen_dates, twenty_seventeen_dates, twenty_eighteen_dates, twenty_nineteen_dates, twenty_twenty_dates)

	# cartesian product of dates
	dates_and_stats <- expand.grid(d = dates, s = stats)

	# function to generate URLs using cartesian product dates_and_stats
	generateTeamStatsURLs <- function(s,d) {
	  full_team_stats_url <- paste(url_one,s,url_two,d,sep="")
	}

	# use apply to pass cartesian product of stats and dates through a function expecting two arguments
	full_team_stats_url_strings <- apply(dates_and_stats, 1, function(x) do.call(generateTeamStatsURLs, as.list(x)))

	# function to scrape data
	teamStatsScrape <- function(x) {
	rankings <- x %>%
	  read_html() %>%
	  html_nodes("table") %>%
	  html_table %>%
	  .[[1]] %>%
	  .[,c(2,3)] %>%
	  rename_at( 2, ~"value" ) %>%
	  mutate(date = sub('.*date=', '', x),
			 stat = sub('\\?.*', '',sub('.*stat/', '', x)))
	}  

	# pass urls through scrape function in parallel 
	plan(multiprocess)
	team_stats_data_list <- future_lapply(full_team_stats_url_strings, function(x) teamStatsScrape(x))

	# combine list of data to one data frame
	nfl_team_stats_combined <- do.call(rbind, team_stats_data_list)

	# widen data 
	nfl_team_stats_transform <<- nfl_team_stats_combined %>%
	  mutate(year = substr(date, 1, 4),
			 ds = date(date)) %>%
	  group_by(Team, year, stat) %>%
	  arrange(ds) %>%
	  mutate(week = rank(ds, ties.method = 'first')) %>%
	  ungroup() %>%
	  spread(key = stat, value = value) %>%
	  select(-date, -ds) %>%
	  mutate(Team = trimws(tolower(Team)))
}