#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

download_rotogrinders_projections_NBA <- function(date) {
  original_wd <- getwd()
  
  data <- read.csv(file = "https://rotogrinders.com/projected-stats/nba-player.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(data) <- c("player", "salary", "team", "pos", "opp", "ceil", "floor", "fpts")

  # write to file
  setwd(paste0('NBA/data_warehouse/projections/rotogrinders'))
  write.csv(data, file = paste0('roto_', date, '.csv'), row.names = F)
  
  setwd(original_wd)
}

