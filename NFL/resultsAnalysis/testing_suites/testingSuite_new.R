if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

####### DESCRIPTION #########
# In this file we compute the PnLs of lineups for testing purposes.
# Special notes for contest.entry.fee: for week 9, use $4 in lieu of $3 and for week 10, $27 in lieu of $20


####### IMPORT LIBRARIES #########
library('stringr')


####### SET PARAMETER VALUES #########
week.lo <- 2
week.hi <- 17

contest.entry.fee <- "$20"
wk.4 <- c(10,16) # weeks where $3 contest was $4
wk.27 <- c(11:15, 18:19) # weeks where $20 contest was $27
wk.50 <- c(16) # weeks where $20 contest was $50
thu_mon.bool <- F # True if using thursday-monday games, False if using only Sunday games

predictions.source <- "_dfn" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
source.actual.fpts <- 'DFN' # 'FC' or 'DFN'

formulation <- 22

overlap.lo <- 4 # overlap.lo and overlap.hi must be the same if exposure.range is not from 1 to 1
overlap.hi <- 4

exposure.range <- seq(from = 0.4, to = 0.4, by = 0) # must be from 1 to 1 if overlap.lo != overlap.hi
exposure.pos.bool <- T # if TRUE then exposure.range is ignored, if FALSE then position exposures (def, wr, rb, te, qb) ignored
exposure.def <- 0.25
exposure.wr <- 0.25
exposure.rb <- 0.75
exposure.te <- 0.75
exposure.qb <- 0.5
exposure.valuewr <- "_valuewrexp_0.025" # "_valuewrexp_0.15" or ""
freqInd <- "" # _FreqInd or ""

num.lineups <- "" # "" or "_numlineups_1000"

missing.data.1M.contest.wk <- c(10) # enter weeks that we don't have complete data for in the $1M to 1st contest
missing.data.50k.contest.wk <- c() # enter weeks that we don't have complete data for in the $50k to 1st contest


####### MISCELLANEOUS #########
# for printing stuff later
if (contest.entry.fee == '$3' | contest.entry.fee == '$4') {
  contest.name <- "$50K"
} else {
  contest.name <- "$1M"
}

####### INITALIZE PNL MATRIX FOR STORING RESULTS #########
if (week.lo != week.hi & overlap.lo == overlap.hi & length(exposure.range) == 1) {
  weeks.count <- week.hi-week.lo+1
  pnlMatrix <- matrix(data = NA, nrow = weeks.count, ncol = 2, dimnames = list(NULL, c("Week","PnL")))
  pnlMatrix[1:weeks.count,'Week'] <- week.lo:week.hi
} else if (length(exposure.range) == 1) {
  pnlMatrix <- matrix(data = NA, nrow = 9, ncol = 2, dimnames = list(NULL, c("Overlap","PnL")))
  pnlMatrix[1:9,'Overlap'] <- 1:9
} else {
  pnlMatrix <- matrix(data = NA, nrow = 10, ncol = 2, dimnames = list(NULL, c("Exposure","PnL")))
  pnlMatrix[1:10, 'Exposure'] <- exposure.range
}

# Loop through weeks
par(mfrow=c(2,2))
for (week.num in week.lo:week.hi) {
  if ((week.num %in% missing.data.1M.contest.wk & contest.entry.fee == '$20') | (week.num %in% missing.data.50k.contest.wk & contest.entry.fee == '$3')) {
    # do nothing
  } else {
    ####### LOAD FULL CONTEST RESULTS #########
    if (thu_mon.bool==F & contest.entry.fee=='$3' & week.num %in% wk.4) {
      full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", '$4', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
    } else if (thu_mon.bool==F & contest.entry.fee=='$20' & week.num %in% wk.27) {
      full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", '$27', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
    } else if (thu_mon.bool==F & contest.entry.fee=='$20' & week.num %in% wk.50) {
      full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", '$50', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
    } else if (thu_mon.bool==T & contest.entry.fee=='$4') {
      full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/", '$4', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
    } else {
      full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", contest.entry.fee, "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F) 
    }

    ####### IMPORT AND CLEAN DK HISTORICAL FPTS DATA #########
    # Use Fantasy Cruncher for actual fpts data
    if (source.actual.fpts == 'FC') {
      player.performance <- read.csv(file = paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", week.num, ".csv"), stringsAsFactors = F)
      player.performance$Player <- sub(' Sr.','', player.performance$Player)
      player.performance$Player <- sub(' Jr.','', player.performance$Player)
      player.performance$Player.Name <- player.performance$Player # to keep column name consistent
      player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0
      player.performance$Actual.FP <- player.performance$Actual.Score # to keep column name consistent
    }
    # Use DFN for actual fpts data
    else if (source.actual.fpts == 'DFN') {
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
    }

    ######## IMPORT PAYOUT STRUCTURE ########
    if (thu_mon.bool==F & contest.entry.fee=='$3' & week.num %in% wk.4) {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", '$4', "_payout_structure_week", week.num, ".csv") 
    } else if (thu_mon.bool==F & contest.entry.fee=='$20' & week.num %in% wk.27) {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", '$27', "_payout_structure_week", week.num, ".csv") 
    } else if (thu_mon.bool==F & contest.entry.fee=='$20' & week.num %in% wk.50) {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", '$50', "_payout_structure_week", week.num, ".csv") 
    } else if (thu_mon.bool==T & contest.entry.fee=='$4') {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_thu-mon/", '$4', "_payout_structure_week", week.num, ".csv") 
    } else {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", contest.entry.fee, "_payout_structure_week", week.num, ".csv") 
    }
    payout.data <- read.csv(file = file.name, stringsAsFactors = F)

    ######## LOOP THROUGH OVERLAP AND EXPOSURE PARAMETER VALUES ########
    for (k in overlap.lo:overlap.hi) {
      
      for (exposure in exposure.range) {
        
        ####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
        # if (thu_mon.bool==F & exposure.pos.bool == F) {
        #   file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, ".csv")
        # } else if (thu_mon.bool==T & exposure.pos.bool == F) {
        #   file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, ".csv")
        # } else {
        #   file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv")
        # }
        
        # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14 (baseline)
        # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14
        # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv")
        file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/vary_numlineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, "_200lineups_test.csv")
        
        lineups <- read.csv(file = file.name, stringsAsFactors = F)

        ######## CALCULATE FPTS FOR EACH LINEUP ########
        for (i in 1:ncol(lineups)) {
          lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
          lineups[,i] <- sub(' Sr.','', lineups[,i])
          lineups[,i] <- sub(' Jr.','', lineups[,i]) 
        }
        lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)

        total_results <- player.performance[,c('Player.Name', 'Actual.FP', 'Salary')]
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

        ######## ADD TO PNL MATRIX ########
        if (contest.entry.fee=='$3' & week.num %in% wk.4) {
          temp.contest.entry.fee <- '$4'
        } else if (contest.entry.fee=='$20' & week.num %in% wk.27) {
          temp.contest.entry.fee <- '$27'
        } else if (contest.entry.fee=='$20' & week.num %in% wk.50) {
          temp.contest.entry.fee <- '$50'
        } else {
          temp.contest.entry.fee <- contest.entry.fee
        }

        temp.pnl <- sum(lineups$payout) - as.numeric(substring(temp.contest.entry.fee, 2)) * nrow(lineups)
        if (week.lo != week.hi & overlap.lo == overlap.hi & length(exposure.range) == 1) {
          pnlMatrix[week.num-week.lo+1, 'PnL'] <- temp.pnl
        } else if (length(exposure.range) == 1) {
          pnlMatrix[k, 'PnL'] <- temp.pnl
        } else {
          pnlMatrix[pnlMatrix[,'Exposure']==exposure, 'PnL'] <- temp.pnl
        }

        ######## FUNCTION FOR CALCULATING TOTAL PNL OF LINEUPS ########
        # won't be exact b/c not accounting for ties
        calculatePnL <- function(numberEntries, lineups, temp.contest.entry.fee) {
          lineups <- lineups[1:numberEntries,]  
          return(sum(lineups$payout) - as.numeric(substring(temp.contest.entry.fee, 2)) * nrow(lineups))
        }
  
        ######## PNL VS NUMBER OF LINEUPS ########
        pnls <- rep(0,nrow(lineups))
        for (i in 1:length(pnls)) {
          pnls[i] <- calculatePnL(nrow(lineups)-i+1, lineups, temp.contest.entry.fee)
        }

        numLineups <- seq(from = nrow(lineups), to = nrow(lineups)-length(pnls)+1)
        plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l")
        abline(h=0, col = "red")
      }
    }

    # print
    if (week.lo != week.hi & overlap.lo == overlap.hi & length(exposure.range) == 1) {
      if (exposure.pos.bool == F) {
        print(paste0('Formulation: ', formulation, '; Overlap: ', overlap.lo, '; Exposure: ', exposure.range[1], '; Contest: ', contest.name))
      } else {
        print(paste0('Formulation: ', formulation, '; Overlap: ', overlap.lo, '; Exposures: ', exposure.def, ', ', exposure.wr, ', ', exposure.rb, ', ', exposure.te, ', ', exposure.qb, '; Contest: ', contest.name))
      }
    } else {
      print(paste0('Week: ', week.num, '; Overlap: ', overlap.lo, '; Formulation: ', formulation))
    }
    print(pnlMatrix)
  }
}

# write lineups to file
# if (exposure.pos.bool == F) {
#   write.csv(lineups, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, "_results.csv"), row.names = F)
# } else {
#   write.csv(lineups, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, "_results.csv"), row.names = F) 
# }

# plotting across weeks
# par(mfrow=c(1,1))
# pnlMatrix <- as.data.frame(pnlMatrix)
# plot(pnlMatrix$Week, pnlMatrix$PnL, ylim = c(-2000,2000), type = 'b', xlab = "Week", ylab = "PnL", main = paste0("Form4-Ovrlap4-Exp0.4 (Contest: ", contest.name, ")"))
# abline(0,0)

# number of placing lineups
# paste0("Number of Placing Lineups: ", sum(lineups$payout != 0))
# hist(lineups$payout[lineups$payout>0], breaks=20)

# number of placing lineups
pnlMatrix
sum(pnlMatrix[,2], na.rm = T)
# sum(pnlMatrix[c(1,3:15),2], na.rm = T)

