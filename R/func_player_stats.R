nflPlayerStats <- function(positions, years, weeks) {
	# partial url strings
	url_one <- 'http://www.footballdb.com/fantasy-football/index.html?pos='
	url_two <- '&yr='
	url_three <- '&wk='
	url_four <- '&rules=1'

	# cartesian product of positions, weeks and years
	pos_weeks_years <- expand.grid(p = positions, y = years, w = weeks)

	# function to generate full url strings
	genFootballDBURLs <- function(p,y,w) {
	  full_url <- paste(url_one, p, url_two, y, url_three, w, url_four, sep = '')
	}

	# run cartesian product of positions, weeks and years through url generator
	football_db_urls <- gsub(' ', '', apply(pos_weeks_years, 1, function(x) do.call(genFootballDBURLs, as.list(x))))

	# create a function to use first row as column names
	firstRowNames <- function(df) {
	  names(df) <- as.character(unlist(df[1,]))
	  df[-1,]
	}

	# create function to scrape data 
	scrapeFootballDB <- function (x) {
	  
	  player_stats <- x %>%
		read_html() %>%
		xml_find_all("//table") %>%
		html_table() %>%
		.[[1]] %>%
		.[-1,]

	  names(player_stats) <- c("player","game","fp","pass_att","pass_comp","pass_yards","pass_td","int","pass_two_pt",
							 "rush_att","rush_yards","rush_td","rush_two_pt","rec","rec_yards","rec_td","rec_two_pt",
							 "fumble","fumble_td")

	  player_stats <- player_stats %>%
		mutate(year = str_sub(x, start= 65, end = 68),
			   week = sub('\\&r.*', '',sub('.*wk=', '', x)),
			   pos = sub('\\&yr.*', '',sub('.*pos=', '', x)),
			   name = stri_reverse(substring(gsub('^.*?\\.', '', stri_reverse(player)), 2)))
	}

	# pass urls through scrape function in parallel 
	plan(multiprocess)
	player_stats_data_list <- future_lapply(football_db_urls, function(x) scrapeFootballDB(x))

	# combine list of data to one data frame
	player_stats_combined <<- do.call(rbind, player_stats_data_list)	
}