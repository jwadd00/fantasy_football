nflVegas <- function(teams, years) {
	# set up partial url strings
	url_string1 <- 'http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nfl/teams/pastresults/'
	url_string2 <- '-'
	url_string3 <- '/team'
	url_string4 <- '.html'

	# set up function to generate full url's
	url_gen <- function(t,y) {
	  full_url <- paste(url_string1,y,url_string2,y+1,url_string3,t,url_string4,sep="")
	}

	# create sequences team numbers and years
	arguments <- expand.grid(t = teams, y = years) # cartesian produce of teams and years

	# use apply to pass cartesian product of teams and years through a function expecting two arguments
	full_url_strings <- apply(arguments, 1, function(x) do.call(url_gen, as.list(x)))

	# set up function to scrape vegas data 
	scrape_vegas <- function (x) {
	  read_page <- read_html(x)
	  table_list <- read_page %>%
		html_nodes(xpath="//h3[contains(., 'Regular')]/following-sibling::table")
	  df <- table_list[[1]] %>%
		html_table()
	  names(df) <- c('game_date', 'opp', 'score', 'week', 'line', 'o_u')
	  team <- substr(df[1,5], start=1, stop = 3)
	  df <- df %>%
		slice(2:n()) %>%
		mutate(team = trimws(tolower(rep(team, nrow(df)-1))),
			   week = sub(".*? (.+)", "\\1", week),
			   year = paste('20',str_sub(game_date, start= -2),sep=''),
			   line = sub(".*? (.+)", "\\1", line),
			   o_u = sub(".*? (.+)", "\\1", o_u)) %>%
		filter(week != 'BYE')
	  }

	# run scrape function for all url's that were created
	plan(multiprocess)
	vegas_list <- future_lapply(full_url_strings, function(x) scrape_vegas(x))

	# combine list of data to one data frame
	nfl_vegas_history <<- do.call(rbind, vegas_list)
}