#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we compute the PnLs of lineups in Double Up contests.


####### IMPORT LIBRARIES #########
library('stringr')


####### SET PARAMETER VALUES #########
week.lo <- 2
week.hi <- 15

contest.name <- 'massive$25doubleup'
contest.entry.fee <- "$25"
thu_mon.bool <- T # True if using thursday-monday games, False if using only Sunday games

predictions.source <- "_dfn" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
source.actual.fpts <- 'DFN' # 'FC' or 'DFN'

formulation <- 4

overlap.lo <- 4 # overlap.lo and overlap.hi must be the same if exposure.range is not from 1 to 1
overlap.hi <- 4

exposure.range <- seq(from = 0.4, to = 0.4, by = 0) # must be from 1 to 1 if overlap.lo != overlap.hi
exposure.pos.bool <- F # if TRUE then exposure.range is ignored, if FALSE then position exposures (def, wr, rb, te, qb) ignored
exposure.def <- 0.4
exposure.wr <- 0.2
exposure.rb <- 0.4
exposure.te <- 0.4
exposure.qb <- 0.4

freqInd <- "" # _FreqInd or ""

num.lineups <- "" # "" or "_numlineups_1000"


# Init
num.cashing.mat <- as.data.frame(matrix(data = NA, nrow = week.hi-week.lo+1, ncol = 2, dimnames = list(NULL, c("Week","Num.Cashing"))))
num.cashing.mat[,'Week'] <- week.lo:week.hi

# load file
if (thu_mon.bool == F) {
  cashing.dat <- read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/contest_",contest.name,"_cashing_sun.csv"), stringsAsFactors = F)  
} else {
  cashing.dat <- read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/contest_",contest.name,"_cashing_thu-mon.csv"), stringsAsFactors = F)  
}

# Init
num.cashing.lineup.index.mat <- as.data.frame(matrix(data = NA, nrow = week.hi-week.lo+1, ncol = 151, dimnames = list(NULL, c("Week",paste0('NumLineups',1:150)))))
num.cashing.lineup.index.mat[,'Week'] <- week.lo:week.hi


# Loop through weeks
par(mfrow=c(1,2))
for (week.num in week.lo:week.hi) {
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
  
  ######## LOOP THROUGH OVERLAP AND EXPOSURE PARAMETER VALUES ########
  for (k in overlap.lo:overlap.hi) {
    for (exposure in exposure.range) {
      ####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
      if (thu_mon.bool==F & exposure.pos.bool == F) {
        file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, ".csv") 
      } else if (thu_mon.bool==T & exposure.pos.bool == F) {
        file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, ".csv") 
      } else {
        file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") 
      }
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
      
      plot(lineups$total, main = paste0("Week ", week.num, ", Overlap ", k), xlab = "Lineup Index", ylab = "Lineup FPts")
    }
  }
  
  # plot
  hist(lineups$total)
  
  # fill num.cashing.mat
  num.cashing.mat[week.num-week.lo+1,'Num.Cashing'] <- sum(lineups$total > cashing.dat[week.num,'Fpts.Cash'])
  
  ######## PNL VS NUMBER OF LINEUPS ########
  # calculatePnL <- function(numberEntries, lineups) {
  #   lineups <- lineups[1:numberEntries,]
  #   temp.num.cashing <- sum(lineups$total > cashing.dat[week.num,'Fpts.Cash'])
  #   temp.pnl <- num.cashing.mat$Num.Cashing*(as.numeric(substring(contest.entry.fee, 2))*2) - (150-num.cashing.mat$Num.Cashing)*as.numeric(substring(contest.entry.fee, 2))
  #   return(sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))
  # }
  # pnls <- rep(0,nrow(lineups))
  # for (i in 1:length(pnls)) {
  #   pnls[i] <- calculatePnL(nrow(lineups)-i+1, lineups)
  # }
  # numLineups <- seq(from = nrow(lineups), to = nrow(lineups)-length(pnls)+1)
  # plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l")
  # abline(h=0, col = "red")
  for (p in 1:nrow(lineups)) {
    num.cashing.lineup.index.mat[week.num-week.lo+1,p+1] <- sum(lineups$total[1:p] > cashing.dat[week.num,'Fpts.Cash'])
  }
}


######## COMPUTE PNL ########
# pnl for each week's full set of 150 lineups
num.cashing.mat$PnL <- num.cashing.mat$Num.Cashing*(as.numeric(substring(contest.entry.fee, 2))*2) - 150*as.numeric(substring(contest.entry.fee, 2))

# pnl for each week's first n lineups
pnl.cashing.lineup.index.mat <- num.cashing.lineup.index.mat
for (i in 1:nrow(pnl.cashing.lineup.index.mat)) {
  for (j in 1:nrow(lineups)) {
    pnl.cashing.lineup.index.mat[i,j+1] <- num.cashing.lineup.index.mat[i,j+1]*(as.numeric(substring(contest.entry.fee, 2))*2) - j*as.numeric(substring(contest.entry.fee, 2))
  }
}


######## VISUALIZATIONS ########
par(mfrow=c(1,1))

# number of lineups that cash
plot(week.lo:week.hi, num.cashing.mat[,'Num.Cashing'], type = 'b', xlab = 'Week', ylab = 'Num Cashing Lineups', main = paste0('Num Cashing Lineups (',contest.name,', Thu = ',thu_mon.bool,')'))

# PnL (full set of 150 lineups)
plot(week.lo:week.hi, num.cashing.mat$PnL, type = 'b', xlab = 'Week', ylab = 'PnL', main = paste0('PnL (',contest.name,', Thu = ',thu_mon.bool,')'))
abline(0,0)
print(paste0('Total PnL Over All Weeks: ',sum(num.cashing.mat$PnL, na.rm = T)))

# PnL (first n lineups)
par(mfrow=c(2,2))
for (i in week.lo:week.hi) {
  if (is.na(pnl.cashing.lineup.index.mat[i-week.lo+1,2]) == F) {
    plot(1:150, pnl.cashing.lineup.index.mat[i-week.lo+1,2:(nrow(lineups)+1)], xlab = 'Lineup Index', ylab = 'PnL', main = paste0('Wk ',i,', ',contest.name,', Thu = ',thu_mon.bool), type = 'l')
    abline(0,0)
  }
}

# Total PnL Over All Weeks (first n lineups)
pnl.total.mat <- NULL
for (j in 1:nrow(lineups)) {
  pnl.total.mat <- c(pnl.total.mat, sum(pnl.cashing.lineup.index.mat[,j+1], na.rm = T)) 
}
par(mfrow=c(1,1))
plot(1:150, pnl.total.mat, xlab = 'Lineup Index', ylab = 'PnL', main = paste0('PnL Over All Weeks, ',contest.name,', Thu = ',thu_mon.bool), type = 'l')
abline(0,0)
