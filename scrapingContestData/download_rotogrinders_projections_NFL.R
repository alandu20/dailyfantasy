#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

download_rotogrinders_projections_NFL <- function(date) {
  original_wd <- getwd()
  
  # offense
  qb.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-qb.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(qb.data) <- c("player", "salary", "team", "pos", "opp", "ceil", "floor", "fpts")
  
  flex.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-flex.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(flex.data) <- c("player", "salary", "team", "pos", "opp", "ceil", "floor", "fpts")
  
  off.data <- rbind(qb.data, flex.data)
  
  # defense
  def.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-defense.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(def.data) <- c("player", "salary", "team", "pos", "opp", "ceil", "floor", "fpts")
  
  # write to file
  setwd(paste0('NFL/data_warehouse/projections/rotogrinders'))
  write.csv(off.data, file = paste0('roto_offense_', date, '.csv'), row.names = F)
  write.csv(def.data, file = paste0('roto_defense_', date, '.csv'), row.names = F)
  
  setwd(original_wd)
}

