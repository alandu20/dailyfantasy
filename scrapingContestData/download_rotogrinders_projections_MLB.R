#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

download_rotogrinders_projections_MLB <- function(date) {
  original_wd <- getwd()
  
  pitchers <- read.csv(file = "https://rotogrinders.com/projected-stats/mlb-pitcher.csv?site=draftkings", stringsAsFactors = F, header = F)
  names(pitchers) <- c("Name", "Salary", "Team", "Position", "OppTeam", "REMOVE1", "REMOVE2", "Projections")
  pitchers$REMOVE1 <- NULL
  pitchers$REMOVE2 <- NULL
  
  
  hitters <- read.csv(file = "https://rotogrinders.com/projected-stats/mlb-hitter.csv?site=draftkings", stringsAsFactors = F, header = F)
  names(hitters) <- c("Name", "Salary", "Team", "Position", "OppTeam", "REMOVE1", "REMOVE2", "Projections")
  hitters$REMOVE1 <- NULL
  hitters$REMOVE2 <- NULL
  
  setwd(paste0('MLB/data_warehouse/projections/rotogrinders'))
  
  
  write.csv(pitchers, file = paste0('pitchers-', date, '.csv'), row.names = F)
  write.csv(hitters, file = paste0('hitters-', date, '.csv'), row.names = F)
  
  setwd(original_wd)
}

