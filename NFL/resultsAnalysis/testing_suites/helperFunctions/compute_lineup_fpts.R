####### DESCRIPTION #########
# This file loads the full contest results into R
# Given: Player_Performance_DF, Lineups
# Returns the Lineups DF with an added column of the total fpts per lineup
# If the file does not exist, returns 0


compute_lineup_fpts <- function(player_performance_df, lineups) {
  ######## CALCULATE FPTS FOR EACH LINEUP ########
  for (i in 1:ncol(lineups)) {
    lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
    lineups[,i] <- sub(' Sr.','', lineups[,i])
    lineups[,i] <- sub(' Jr.','', lineups[,i]) 
  }
  lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)
  
  total_results <- player_performance_df[,c('Player.Name', 'Actual.FP', 'Salary')]
  if (source.actual.fpts == 'DFN') {
    total_results <- rbind(total_results, player.performance.def[,c('Player.Name', 'Actual.FP', 'Salary')])
  }
  lineups$total <- 0
  
  for (index in 1:nrow(lineups)){
    row <- t(lineups[index,])
    colnames(row) <- 'Player.Name'
    row <- merge(row, total_results, by = 'Player.Name')
    lineups$total[index] <- sum(row$Actual.FP)
  }
  
  return(lineups)
}

