nflWeather <- function (weeks, years) {
	# partial url strings
	url_one <- "http://www.nflweather.com/en/week/"
	url_two <- "/week-"
	url_three <- "/"
	
	weather_weeks_and_years <- expand.grid(y = years, w = weeks) 

	# function to pass weeks and years through for full url generation
	generateWeatherURLs <- function(y,w) {
	  full_weather_url <- paste(url_one,y,url_two,w,url_three,sep="")
	}

	# use apply to pass cartesian product of weeks and years through a function expecting two arguments
	full_weather_url_strings <- apply(weather_weeks_and_years, 1, function(x) do.call(generateWeatherURLs, as.list(x)))

	# function to scrape data 
	scrapeWeather <- function (x) {
	  scraped_weather <- x %>%
		read_html() %>%
		html_nodes(xpath='/html/body/div[1]/div[4]/div/div[1]/div[7]/div/table') %>%
		html_table() %>%
		.[[1]] %>%
		.[,c(2,6,10,12)] %>%
		mutate(id = paste(Away, Home, sep ='')) %>%
		melt(., id=c('id','Forecast','Wind')) %>%
		dplyr::rename(team = value, forecast = Forecast, wind = Wind) %>%
		select(team, forecast, wind) %>%
		mutate(year = str_sub(x, start= 35, end = 38),
			   week = str_sub(sub('.*\\-', '', x), start = -3, end = -2),
			   wind = as.numeric(gsub( "m.*$", "",wind)),
			   temperature = ifelse(forecast == 'DOME', 71, as.numeric(gsub( "f.*$", "", forecast))),
			   weather = sub(".*? (.+)", "\\1", forecast),
			   team = trimws(tolower(team))) %>%
		select(year, week, team, weather, temperature, wind)
	}


	# pass urls through scrape function in parallel 
	plan(multiprocess)
	weather_data_list <- future_lapply(full_weather_url_strings, function(x) scrapeWeather(x))

	# combine list of data to one data frame
	dfs_weather_nfl <<- do.call(rbind, weather_data_list)
}