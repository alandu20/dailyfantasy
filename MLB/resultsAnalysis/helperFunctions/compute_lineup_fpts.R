####### DESCRIPTION #########
# This file loads the full contest results into R
# Given: Player_Performance_DF, Lineups
# Returns the Lineups DF with an added column of the total fpts per lineup
# If the file does not exist, returns 0


compute_lineup_fpts <- function(player_performance_df, payout_structure, lineups, entry_fee, contest_standings) {
  
  ######## FUNCTION FOR CALCULATING TOTAL PNL OF LINEUPS ########
  # won't be exact b/c not accounting for ties
  calculatePnL <- function(numberEntries, lineups, entry_fee) {
    lineups <- lineups[1:numberEntries,]  
    return(sum(lineups$payout) - as.numeric(substring(entry_fee, 2)) * nrow(lineups))
  }
  
  ######## CALCULATE FPTS FOR EACH LINEUP ########
  
  total_results <- player_performance_df[,c('Player', 'Actual.Score', 'Salary')]
  lineups$total <- 0
  
  for (index in 1:nrow(lineups)){
    row <- t(lineups[index,])
    colnames(row) <- 'Player'
    row <- merge(row, total_results, by = 'Player')
    lineups$total[index] <- sum(row$Actual.Score, na.rm = TRUE)
  }
  
  
  for (i in 1:nrow(lineups)) {
    lineups$place[i] <- which.min(abs(contest_standings$Points-lineups$total[i])) # not precise but good estimate (can be done better probably)
    if (lineups$place[i] > payout_structure$Place_hi[nrow(payout_structure)]) {
      lineups$payout[i] <- 0
    }
    else {
      for (j in 1:nrow(payout_structure)) {
        if (lineups$place[i] >= payout_structure$Place_lo[j] && lineups$place[i] <= payout_structure$Place_hi[j]) {
          lineups$payout[i] <- payout_structure$Payout[j]
          break
        }
      }
    }
  }
  
  #### Calculate PnL
  PnL <- calculatePnL(nrow(lineups), lineups, entry_fee)
  return_var <- list(lineups, PnL)
  return(return_var)
}

