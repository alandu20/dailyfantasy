#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

library(XML)
library(stringr)
library(lme4)
library(dplyr)
library(rvest)

download_player_results <- function(sports_league_acronym, year, week) {
  original_wd <- getwd()
  url_base <- "https://www.fantasycruncher.com/lineup-rewind/draftkings"
  
  url <- paste0(url_base, '/', sports_league_acronym, '/', year, '-week-', week)
  
  webpage_date <- url %>%
    read_html() %>%
    html_node(".period-title") %>%
    html_text()
  
  try( if(webpage_date != date) stop('The Date on the FantasyCruncher Webpage does NOT match desired date'))
  stopifnot(webpage_date == date)
  
  # Scrapes the Table 
  data <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="ff"]') %>%
    html_table()
  data <- data[[1]]
  names(data)[1] <- "Player"
  
  setwd(paste0(original_wd,'/', sports_league_acronym, '/data_warehouse/', date))
  
  write.csv(data, file = 'player_results.csv', row.names = F)
  
  setwd(original_wd)
  return(0)
}
