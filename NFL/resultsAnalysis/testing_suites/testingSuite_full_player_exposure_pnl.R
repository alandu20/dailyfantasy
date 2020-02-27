#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")



####### IMPORT LIBRARIES #########
library('stringr')


####### SET PARAMETER VALUES #########
week.lo <- 2
week.hi <- 15

contest.entry.fee <- "$3" # note: if "$20", we use "$27" after week 9; if "$3", we use "$4" for week 10

predictions.source <- "_dfn" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
source.actual.fpts <- 'FC' # 'FC' or 'DFN'

formulation <- 14

overlap.lo <- 4 # overlap.lo and overlap.hi must be the same if exposure.range is not from 1 to 1
overlap.hi <- 4

exposure.pos.bool <- T # if TRUE then exposure.range is ignored, if FALSE then position exposures ignored
exposure.range <- seq(from = 0.4, to = 0.4, by = 0) # must be from 1 to 1 if overlap.lo != overlap.hi
exposure.def <- seq(from = 0.25, to = 0.75, by = 0.25)
exposure.wr <- seq(from = 0.25, to = 0.75, by = 0.25)
exposure.rb <- seq(from = 0.25, to = 0.75, by = 0.25)
exposure.te <- seq(from = 0.25, to = 0.75, by = 0.25)
exposure.qb <- seq(from = 0.25, to = 0.75, by = 0.25)

freqInd <- "" # _FreqInd or ""

num.lineups <- "" # "" or "_numlineups_1000"

missing.data.1M.contest.wk <- c(10) # enter weeks that we don't have complete data for in the $1M to 1st contest
missing.data.50k.contest.wk <- c() # enter weeks that we don't have complete data for in the $50k to 1st contest


####### MISCELLANEOUS #########
# for printing stuff later
if (contest.entry.fee == '$3') {
  contest.name <- "$50K"
} else {
  contest.name <- "$1M"
}


####### INITALIZE PNL MATRIX FOR STORING RESULTS #########

weeks.count <- week.hi-week.lo+1
exposure_count <- length(exposure.def)*length(exposure.wr)*length(exposure.rb)*length(exposure.te)*length(exposure.qb)
pnlMatrix <- as.data.frame(matrix(data = NA, nrow = (weeks.count*exposure_count), ncol = 3, dimnames = list(NULL, c("Week","Exposure","PnL"))))
for(i in week.lo:week.hi) {
  pnlMatrix[(1 + (i-week.lo)*exposure_count):((i-week.lo+1)*exposure_count),'Week'] <- i
}


# Loop through weeks
par(mfrow=c(2,2))
counter <- 1 

for (week.num in week.lo:week.hi) {
  print(paste0("currently working on week: ", week.num))
  if ((week.num %in% missing.data.1M.contest.wk & contest.entry.fee == '$20') | (week.num %in% missing.data.50k.contest.wk & contest.entry.fee == '$3')) {
    # do nothing
  } else {
    ####### LOAD FULL CONTEST RESULTS #########
    if (contest.entry.fee=='$3' & week.num == 10) {
      full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", '$4', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
    } else if (contest.entry.fee=='$20' & week.num > 9) {
      full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", '$27', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
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
    if (contest.entry.fee=='$3' & week.num == 10) {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", '$4', "_payout_structure_week", week.num, ".csv") 
    } else if (contest.entry.fee=='$20' & week.num > 9) {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", '$27', "_payout_structure_week", week.num, ".csv") 
    } else {
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", contest.entry.fee, "_payout_structure_week", week.num, ".csv") 
    }
    payout.data <- read.csv(file = file.name, stringsAsFactors = F)
    
    ######## LOOP THROUGH OVERLAP AND EXPOSURE PARAMETER VALUES ########
    for (k in overlap.lo:overlap.hi) {
      
      for (exposure in exposure.range) {
        for (def_exposure in exposure.def) {
          for (wr_exposure in exposure.wr) {
            for (rb_exposure in exposure.rb) {
              for (te_exposure in exposure.te) {
                for (qb_exposure in exposure.qb) {
                  ####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
                  file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/player_exposure/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", def_exposure, "_wrexp_", wr_exposure, "_rbexp_", rb_exposure, "_teexp_", te_exposure,"_qbexp_", qb_exposure, num.lineups, ".csv") 
                  if(!file.exists(file.name)) { 
                    pnlMatrix[counter, 'PnL'] <- NA
                    pnlMatrix[counter, 'Exposure'] <- paste0("defexp_", def_exposure, " wrexp_", wr_exposure, " rbexp_", rb_exposure, " teexp_", te_exposure," qbexp_", qb_exposure) 
                    print(paste0("DO NOT HAVE THIS FILE: ", "defexp_", def_exposure, " wrexp_", wr_exposure, " rbexp_", rb_exposure, " teexp_", te_exposure," qbexp_", qb_exposure))
                    counter <- counter + 1
                  } else {
                    lineups <- read.csv(file = file.name, stringsAsFactors = F)
                    
                    ######## CALCULATE FPTS FOR EACH LINEUP ########
                    for (i in 1:ncol(lineups)) {
                      lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
                      lineups[,i] <- sub(' Sr.','', lineups[,i])
                      lineups[,i] <- sub(' Jr.','', lineups[,i]) 
                    }
                    lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)
                    
                    total_results <- player.performance[,c('Player.Name', 'Actual.FP')]
                    if (source.actual.fpts == 'DFN') {
                      total_results <- rbind(total_results, player.performance.def[,c('Player.Name', 'Actual.FP')])
                    }
                    lineups$total <- 0
                    
                    for (index in 1:nrow(lineups)){
                      row <- t(lineups[index,])
                      colnames(row) <- 'Player.Name'
                      row <- merge(row, total_results, by = 'Player.Name')
                      lineups$total[index] <- sum(row$Actual.FP)
                    }
                    
                    #plot(lineups$total, main = paste0("Week ", week.num, ", Overlap ", k), xlab = "Lineup Index", ylab = "Lineup FPts")
                    
                    
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
                    if (contest.entry.fee=='$3' & week.num == 10) {
                      temp.contest.entry.fee <- '$4'
                    } else if (contest.entry.fee=='$20' & week.num > 9) {
                      temp.contest.entry.fee <- '$27'
                    } else {
                      temp.contest.entry.fee <- contest.entry.fee
                    }
                    
                    temp.pnl <- sum(lineups$payout) - as.numeric(substring(temp.contest.entry.fee, 2)) * nrow(lineups)
                    pnlMatrix[counter, 'PnL'] <- as.numeric(temp.pnl)
                    pnlMatrix[counter, 'Exposure'] <- paste0("defexp_", def_exposure, " wrexp_", wr_exposure, " rbexp_", rb_exposure, " teexp_", te_exposure," qbexp_", qb_exposure) 
                    
                    counter <- counter + 1
                  }
                }
              }
            }
          }
        } 
        
      }
    }
  }
}

pnlMatrix$profit <- as.numeric(pnlMatrix$PnL > 0)
pnlMatrix$loss <- as.numeric(pnlMatrix$PnL < 0)
num.games <- c(13,14,14,13,12,13,13,11,11,12,12,12,13,14,13)
many_games <- pnlMatrix[pnlMatrix$Week != 5,] 
many_games <- many_games[many_games$Week != 8,] 
many_games <- many_games[many_games$Week != 9,] 
many_games <- many_games[many_games$Week != 10,]  
many_games <- many_games[many_games$Week != 11,]  
many_games <- many_games[many_games$Week != 12,]  

many_games <- many_games[many_games$Week != 1,] 
many_games <- many_games[many_games$Week != 4,] 
many_games <- many_games[many_games$Week != 6,] 
many_games <- many_games[many_games$Week != 7,] 
many_games <- many_games[many_games$Week != 13,] 
many_games <- many_games[many_games$Week != 15,] 

results <- unique(within(pnlMatrix, {
     max_loss <- ave(PnL, Exposure, FUN = min)
     PnL <- ave(PnL, Exposure, FUN = sum)
     count_profit <- ave(profit, Exposure, FUN = sum)
     count_loss <- ave(loss, Exposure, FUN = sum)
     rm(Week, profit, loss)
}))

temp <- subset(many_games, Exposure == "defexp_0.25 wrexp_0.5 rbexp_0.5 teexp_0.25 qbexp_0.25")

#save(pnlMatrix, results, file="player_exposure_pnl.RData")
