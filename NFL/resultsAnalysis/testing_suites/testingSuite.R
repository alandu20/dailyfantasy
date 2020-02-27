#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we compute the PnLs of lineups for testing purposes.
# Special notes for contest.entry.fee: for week 9, use $4 in lieu of $3 and for week 10, $27 in lieu of $20


####### IMPORT LIBRARIES #########
library('stringr')


####### SET PARAMETER VALUES #########
week.lo <- 13
week.hi <- 13
contest.entry.fee <- "$3"
predictions.source <- "_actual" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
formulation <- 4
overlap.lo <- 8 # overlap.lo and overlap.hi must be the same if exposure.range is not from 1 to 1
overlap.hi <- 8
exposure.range <- seq(from = 1, to = 1, by = 0.1) # must be from 1 to 1 if overlap.lo != overlap.hi
freqInd <- "" # _FreqInd or ""
num.lineups <- "_numlineups_10000" # "" or "_numlineups_1000"


####### INITALIZE PNL MATRIX FOR STORING RESULTS #########
if (length(exposure.range) == 1) {
  pnlMatrix <- matrix(data = NA, nrow = 9, ncol = 2, dimnames = list(NULL, c("Overlap","PnL")))
  pnlMatrix[1:9,'Overlap'] <- 1:9  
} else {
  pnlMatrix <- matrix(data = NA, nrow = 10, ncol = 2, dimnames = list(NULL, c("Exposure","PnL")))
  pnlMatrix[1:10, 'Exposure'] <- exposure.range
}

# Loop through weeks
week.num <- 12
for (week.num in week.lo:week.hi) {
  
  ####### LOAD FULL CONTEST RESULTS #########
  full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", contest.entry.fee, "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
  
  ####### IMPORT AND CLEAN DK HISTORICAL FPTS DATA #########
  # player.performance <- read.csv(file = paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", week.num, ".csv"), stringsAsFactors = F)
  # player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0
  # 
  # player.performance$Player <- sub(' Sr.','', player.performance$Player)
  # player.performance$Player <- sub(' Jr.','', player.performance$Player)
  
  player.performance <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', week.num, ".csv"), stringsAsFactors = F)
  player.performance.def <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week', week.num, ".csv"), stringsAsFactors = F)
  
  # clean defense names
  temp.def.names <- str_split_fixed(player.performance.def$Player.Name, " ", 3) # split at " "
  for (z in 1:nrow(temp.def.names)) {
    if (temp.def.names[z,3] == "") {
      player.performance.def$Player.Name[z] <- temp.def.names[z,2]
    } else {
      player.performance.def$Player.Name[z] <- temp.def.names[z,3]
    }
  }
  
  player.performance$Actual.FP[is.na(player.performance$Actual.FP)] <- 0 # be careful with this
  player.performance.def$Actual.FP[is.na(player.performance.def$Actual.FP)] <- 0 # be careful with this
  
  ######## IMPORT PAYOUT STRUCTURE ########
  file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", contest.entry.fee, "_payout_structure_week", week.num, ".csv")
  payout.data <- read.csv(file = file.name, stringsAsFactors = F)
  
  ######## LOOP THROUGH OVERLAP PARAMETER VALUES ########
  # k <- overlap.lo
  for (k in overlap.lo:overlap.hi) {
    
    # Loop through exposures
    # exposure <- exposure.range
    for (exposure in exposure.range) {
      
      ####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
      file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, ".csv")
      lineups <- read.csv(file = file.name, stringsAsFactors = F)
      
      ######## CALCULATE FPTS FOR EACH LINEUP ########
      for (i in 1:ncol(lineups)) {
        lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
        lineups[,i] <- sub(' Sr.','', lineups[,i])
        lineups[,i] <- sub(' Jr.','', lineups[,i]) 
      }
      lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)
      
      # total_results <- player.performance[,c('Player', 'Actual.Score')]
      total_results <- player.performance[,c('Player.Name', 'Actual.FP')]
      total_results <- rbind(total_results, player.performance.def[,c('Player.Name', 'Actual.FP')])
      lineups$total <- 0
      
      for (index in 1:nrow(lineups)){
        # index <- 143
        row <- t(lineups[index,])
        # colnames(row) <- 'Player'
        # row <- merge(row, total_results, by = 'Player')
        # lineups$total[index] <- sum(row$Actual.Score)
        colnames(row) <- 'Player.Name'
        row <- merge(row, total_results, by = 'Player.Name')
        lineups$total[index] <- sum(row$Actual.FP)
      }
      
      plot(lineups$total, main = paste0("Week ", week.num, ", Overlap ", k), xlab = "Lineup Index", ylab = "Lineup FPts")
      
    
      ######## CALCULATE PLACE AND PAYOUT FOR EACH LINEUP ########
      # print(paste0("Number of NAs: ", sum(is.na(lineups$total))))
      for (i in 1:nrow(lineups)) {
        lineups$place[i] <- which.min(abs(full.results.data$Points-lineups$total[i])) # not precise but good estimate (can be done better probably)
        if (lineups$place[i] > payout.data$Place_hi[nrow(payout.data)]) {
          lineups$payout[i] <- 0
        }
        else {
          for (j in 1:nrow(payout.data)) {
            if (lineups$place[i] >= payout.data$Place_lo[j] && lineups$place[i] <= payout.data$Place_hi[j]) {
              lineups$payout[i] <- payout.data$Payout[j]
              break
            }
          }
        }
      }
      
      ######## FUNCTION FOR CALCULATING TOTAL PNL OF LINEUPS ########
      # won't be exact b/c not accounting for ties
      calculatePnL <- function(numberEntries, lineups) {
        lineups <- lineups[1:numberEntries,]  
        return(sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))
      }
      
      ######## ADD TO PNL MATRIX ########
      paste0("Total PnL: ", sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))
      if (length(exposure.range) == 1) {
        pnlMatrix[k, 'PnL'] <- sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups)
      } else {
        pnlMatrix[pnlMatrix[,'Exposure']==exposure, 'PnL'] <- sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups)
      }
      
      ######## PNL VS NUMBER OF LINEUPS ########
      pnls <- rep(0,nrow(lineups))
      for (i in 1:length(pnls)) {
        pnls[i] <- calculatePnL(nrow(lineups)-i+1, lineups)
      }
  
      numLineups <- seq(from = nrow(lineups), to = nrow(lineups)-length(pnls)+1)
      plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l")
      abline(h=0, col = "red")
    }
  }
  
  # print
  print(paste0('Week: ', week.num, '; Overlap: ', overlap.lo, '; Formulation: ', formulation))
  print(pnlMatrix)
  # if (length(exposure.range) == 1) {
  #   saveRDS(pnlMatrix, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", week.num, predictions.source, "_formulation", formulation, "_exposure_", 1, ".rds"))
  # } else {
  #   saveRDS(pnlMatrix, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", week.num, predictions.source, "_formulation", formulation, "_overlap_", overlap.lo, ".rds"))
  # }
}

# write lineups to file
write.csv(lineups, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, "_results.csv"), row.names = F)

