nflDFS <- function (weeks, years) {
  # partial url strings
  url1 <- "http://rotoguru1.com/cgi-bin/fyday.pl?week="
  url2 <- "&year="
  url3 <- "&game=fd&scsv=1"

  # function to pass weeks and years through for full url generation
  generateURL <- function(w,y) {
    full_url <- paste(url1,w, url2, y, url3, sep = '')
  }

  weeks_and_years <- expand.grid(w = weeks, y = years) # cartesian product of weeks and years

  # generate all urls using a two argument function / pass cartesian product of weeks and years through function
  url_list <- apply(weeks_and_years, 1, function(x) do.call(generateURL, as.list(x)))

  # demonstrate urls
  # url_list[1:3]

  # function to scrape data
  scrape_guru <- function(x) {
    page <- read_html(x) %>%
      html_nodes('pre') %>%
      html_text() %>%
      strsplit(split = '\n') %>%
      unlist() %>%
      .[. != ""] %>%
      colsplit(., split = ';', names = c('week', 'year','gid','player','pos','team','h_a','opp','fdp','salary')) %>%
      .[-1,]
  }  

  # execute scrape function for every url
  plan(multiprocess)
  guru_data_lapply <- future_lapply(url_list, function(x) scrape_guru(x))

  # map to dataframe
  guru_data_combined <- do.call(rbind, guru_data_lapply)

  # columns to convert to numeric
  numeric_columns <- c('fdp','salary')

  dfs_logs_nfl <<- guru_data_combined %>% 
    mutate_if(is.factor, as.character) %>% # convert all columns from factor to character
	mutate(player = sapply(strsplit(player, split=", "),function(x) 
                                       {paste(rev(x),collapse=" ")}), # last, first to first last 
	  team = ifelse(team == 'sdg', 'lac',
					ifelse(team == 'stl', 'lar',
						ifelse(team == 'gnb', 'gb',
							ifelse(team == 'nwe', 'ne',
								ifelse(team == 'kan', 'kc', 
									ifelse(team == 'tam', 'tb',
										ifelse(team == 'sfo', 'sf',
											ifelse(team == 'nor', 'no', team)))))))),
	  opp = ifelse(opp == 'sdg', 'lac',
					ifelse(opp == 'stl', 'lar',
						ifelse(opp == 'gnb', 'gb',
							ifelse(opp == 'nwe', 'ne',
								ifelse(opp == 'kan', 'kc', 
									ifelse(opp == 'tam', 'tb',
										ifelse(opp == 'sfo', 'sf',
											ifelse(opp == 'nor', 'no', opp))))))))) %>% # normalize team names 
    mutate_at(.vars = vars(numeric_columns), # convert numeric columns to numeric
              .funs = funs(as.numeric))
}