####### DESCRIPTION #########
# This file loads and cleans the full Player Performance DF into R 
# If the file does not exist, returns 0
# We will use FC data
library('stringr')

load_player_performance <- function(week_number, year, source) {
  library('stringr')
  #Get correct file path  
  if(source == "DFN"){
    file_name <- paste0("optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week", week_number, ".csv")
  } else if (source == "FC") {
    file_name <- paste0("resultsAnalysis/data_warehouse/player_weekly_performance/", year, "/draftkings_player_production_week", week_number, ".csv")
  }
  
  if(!file.exists(file_name)) { 
    print("***** FILE NOT FOUND *****")
    print(paste0('week: ', week_number, ' || year: ', year))
    print(paste0("File Path Attempted: /", file_name))
    return(0)
  } 
  
  if (source == 'FC') {
  player.performance <- read.csv(file = file_name, stringsAsFactors = F)
  player.performance$Player <- sub(' Sr.','', player.performance$Player)
  player.performance$Player <- sub(' Jr.','', player.performance$Player)
  player.performance$Player.Name <- player.performance$Player # to keep column name consistent
  player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0
  player.performance$Actual.FP <- player.performance$Actual.Score # to keep co
  
  # Use DFN for actual fpts data
  } else if (source == 'DFN') {
    player.performance <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', week_number, ".csv"), stringsAsFactors = F)
    player.performance.def <- read.csv(paste0("optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week", week_number, ".csv"), stringsAsFactors = F)
    # clean defense names
    temp.def.names <- str_split_fixed(player.performance.def$Player.Name, " ", 3) # split at " "
    
    for (index in 1:nrow(temp.def.names)) {
      if (temp.def.names[index, 3] == "") {
        player.performance.def$Player.Name[index] <- temp.def.names[index,2]
      } else {
        player.performance.def$Player.Name[index] <- temp.def.names[index,3]
      }
    }
    player.performance$Actual.FP[is.na(player.performance$Actual.FP)] <- 0 # be careful with this
    player.performance.def$Actual.FP[is.na(player.performance.def$Actual.FP)] <- 0 # be careful with this
    player.performance <- rbind(player.performance[,c('Player.Name', 'Actual.FP', 'Salary')], player.performance.def[,c('Player.Name', 'Actual.FP', 'Salary')])
  }
  
  return(player.performance)
}

