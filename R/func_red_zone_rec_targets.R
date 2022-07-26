nflRedZoneTargets <- function(weeks, years) {
	# partial url strings
	url_one <- "http://www.nflsavant.com/targets.php?week="
	url_two <- "&ddlYear="
	url_three <- "&rz=redzone&ddlTeam=&ddlPosition="

	# cartesian product of positions, weeks and years
	pos_weeks_years <- expand.grid(w = weeks, y = years)

	# function to generate full url strings
	genFootballSavantRedZoneURLs <- function(w,y) {
	  full_url <- paste(url_one, w, url_two, y, url_three, sep = '')
	}

	# run cartesian product of positions, weeks and years through url generator
	urls <- apply(pos_weeks_years, 1, function(x) do.call(genFootballSavantRedZoneURLs, as.list(x)))

	# function to scrape nfl savant for red zone receiving targets
	getNFLSavantRedZoneTargets <- function(x) {
	  tryCatch({
	  targets <- x %>%
		read_html() %>%
		html_nodes("table") %>%
		html_table %>%
		as.data.frame(.) %>%
		mutate(week = sub('\\&ddlYe.*', '',sub('.*week=', '', x)),
			   year = sub('\\&rz=r.*', '',sub('.*Year=', '', x))
		)
	  
	  names(targets) <- c('rank', 'name', 'team', 'pos', 'comp', 'rz_targets', 'comp_pct', 'td', 'week', 'year')
	  
	  targets <- targets %>%
		select(rank, name, pos, comp, rz_targets, td, week, year)
	  })
	}

	# pass urls through scrape function in parallel 
	plan(multiprocess)
	red_zone_receiving_targets_data_list <- future_lapply(urls, function(x) getNFLSavantRedZoneTargets(x))

	# combine list of data to one data frame
	red_zone_receiving_targets <<- do.call(rbind, red_zone_receiving_targets_data_list)
}