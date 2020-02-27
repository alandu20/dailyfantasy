#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we compute the fantasy points (no PnL) of lineups for testing purposes.
# Note: change "SET FILE" to whatever path generated lineups are stored at


####### IMPORT LIBRARIES #########
library('stringr')


####### SET PARAMETER VALUES #########
week.lo <- 2
week.hi <- 16

contest.entry.fee <- "$4"
thu_mon.bool <- F # True if using thursday-monday games, False if using only Sunday games

predictions.source <- "_dfn" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
source.actual.fpts <- 'DFN' # 'FC' or 'DFN'

formulation <- 14

overlap <- 4

exposure <- 0.4

exposure.def <- 0.25
exposure.wr <- 0.25
exposure.rb <- 0.75
exposure.te <- 0.75
exposure.qb <- 0.5
exposure.valuewr <- "_valuewrexp_0.15"

freqInd <- "" # _FreqInd or ""

num.lineups <- "" # "" or "_numlineups_1000"

missing.data.1M.contest.wk <- c(10) # enter weeks that we don't have complete data for in the $1M to 1st contest
missing.data.50k.contest.wk <- c() # enter weeks that we don't have complete data for in the $50k to 1st contest


# Init
result.mat <- matrix(data = NA, nrow = week.hi-week.lo+1, ncol = 10, dimnames = list(NULL, c("Week","Mean","Max","Min","Cashing","160-170","170-180","180-190","190-200",">200")))
lineup.fpts.all.wks <- NULL


# Loop through weeks
par(mfrow=c(1,2))
for (week.num in week.lo:week.hi) {
  if ((week.num %in% missing.data.1M.contest.wk & contest.entry.fee == '$20') | (week.num %in% missing.data.50k.contest.wk & contest.entry.fee == '$3')) {
    # do nothing
  } else {
    ####### SET FILE #########
    # for baseline (sun)
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/baseline/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_exposure_", exposure, num.lineups, ".csv") # form 4
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/baseline/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/baseline/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv") # form 15
    
    # for model1 (thu-mon)
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_exposure_", exposure, num.lineups, ".csv") # form 4
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv") # form 15
    
    # for model1 (sun only)
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_exposure_", exposure, num.lineups, ".csv") # form 4
    file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv") # form 15
    
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
    
    ####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
    lineups <- read.csv(file = file.name, stringsAsFactors = F)
    
    ######## CALCULATE FPTS FOR EACH LINEUP ########
    for (i in 1:ncol(lineups)) {
      lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
      lineups[,i] <- sub(' Sr.','', lineups[,i])
      lineups[,i] <- sub(' Jr.','', lineups[,i]) 
    }
    lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)
    
    total_results <- player.performance[,c('Player.Name', 'Actual.FP','Salary')]
    if (source.actual.fpts == 'DFN') {
      total_results <- rbind(total_results, player.performance.def[,c('Player.Name', 'Actual.FP','Salary')])
    }
    lineups$total <- 0
    
    for (index in 1:nrow(lineups)){
      row <- t(lineups[index,])
      colnames(row) <- 'Player.Name'
      row <- merge(row, total_results, by = 'Player.Name')
      lineups$total[index] <- sum(row$Actual.FP)
    }
    
    plot(lineups$total, main = paste0("Week ", week.num, ", Overlap ", overlap), xlab = "Lineup Index", ylab = "Lineup FPts")
    
    hist(lineups$total)
    
    if (week.num != 14) {
      lineup.fpts.all.wks <- c(lineup.fpts.all.wks, lineups$total) 
    }
  }
  
  # compute cashing threshold
  if (thu_mon.bool==T) {
    cashing.dat <- read.csv(file = "resultsAnalysis/data_warehouse/weekly_payout_structure/includes_thu-mon/full_slate_cashing.csv", stringsAsFactors = F) 
  } else {
    cashing.dat <- read.csv(file = "resultsAnalysis/data_warehouse/weekly_payout_structure/contest_1M_cashing.csv", stringsAsFactors = F) 
  }
  cashing.threshold <- cashing.dat$Min[cashing.dat$Week==(week.num-week.lo+1)]
  
  # fill in results matrix
  result.mat[week.num-week.lo+1,1] <- week.num
  result.mat[week.num-week.lo+1,2] <- mean(lineups$total)
  result.mat[week.num-week.lo+1,3] <- max(lineups$total)
  result.mat[week.num-week.lo+1,4] <- min(lineups$total)
  if (is.na(cashing.threshold)) {
    result.mat[week.num-week.lo+1,5] <- sum(lineups$total > 150) 
  } else {
    result.mat[week.num-week.lo+1,5] <- sum(lineups$total > cashing.threshold) 
  }
  result.mat[week.num-week.lo+1,6] <- sum(lineups$total > 160 & lineups$total <= 170)
  result.mat[week.num-week.lo+1,7] <- sum(lineups$total > 170 & lineups$total <= 180)
  result.mat[week.num-week.lo+1,8] <- sum(lineups$total > 180 & lineups$total <= 190)
  result.mat[week.num-week.lo+1,9] <- sum(lineups$total > 190 & lineups$total <= 200)
  result.mat[week.num-week.lo+1,10] <- sum(lineups$total > 200)
}

# View(result.mat)


# load("/Users/Alan/Downloads/player_exposure_pnl_full_slate.RData")



# first plot for paper
result.mat <- as.data.frame(result.mat)
par(mfrow=c(1,1))
min.plot <- min(result.mat$`160-170`,result.mat$`170-180`,result.mat$`180-190`,result.mat$`190-200`,result.mat$`>200`) #result.mat$Cashing,
max.plot <- max(result.mat$`160-170`,result.mat$`170-180`,result.mat$`180-190`,result.mat$`190-200`,result.mat$`>200`) #result.mat$Cashing,
# plot(result.mat$Week, result.mat$Cashing, type = 'b', col = 'black', ylim = c(min.plot,max.plot), main = "asdf")
plot(c(result.mat$Week,17:19), c(result.mat$`160-170`,rep(NA,3)), type = 'b', col = 'red', ylim = c(min.plot,30), main = "Number of Lineups in Specified Fantasy Point Ranges", xlab = "Week", ylab = "Number of Lineups in Range", xaxt = 'n')
axis(side = 1, at = c(2:16), labels = c(2:16), tck=-.05)
# points(result.mat$Week, result.mat$`160-170`, type = 'b', col = 'red')
points(result.mat$Week, result.mat$`170-180`, type = 'b', col = 'blue')
points(result.mat$Week, result.mat$`180-190`, type = 'b', col = 'green')
points(result.mat$Week, result.mat$`190-200`, type = 'b', col = 'orange')
points(result.mat$Week, result.mat$`>200`, type = 'b', col = 'purple')
legend("right", legend = c("160-169.9","170-179.9","180-189.9","190-199.9","200+"), cex = 0.9, fill = c("red","blue","green","orange","purple"))



# second plot for paper
temp <- as.data.frame(matrix(data = NA, nrow = length(lineup.fpts.all.wks), ncol = 2, dimnames = list(NULL, c("Baseline","Model1"))))
temp$Baseline <- lineup.fpts.all.wks
temp$Model1 <- lineup.fpts.all.wks

library(sm)
# quantile(temp$Baseline, 0.75)
temp.baseline <- sort(temp$Baseline[temp$Baseline >= 160])
temp.model1 <- sort(temp$Model1[temp$Model1 >= 160])

length(temp.baseline)
length(temp.model1)

temp.length <- abs(length(temp.baseline)-length(temp.model1))
# temp.baseline <- sort(temp$Baseline[temp$Baseline >= 160])[-c(1:(temp.length))] # need to make equal length
temp.model1 <- sort(temp$Model1[temp$Model1 >= 160])[-c(1:(temp.length))] # need to make equal length

length(temp.baseline)
length(temp.model1)

temp.plot <- as.data.frame(matrix(data = NA, nrow = length(temp.model1), ncol = 2, dimnames = list(NULL, c("Baseline","Model1"))))
temp.plot$Baseline <- temp.baseline
temp.plot$Model1 <- temp.model1

group.index <- rep(1:2, c(length(temp.baseline), length(temp.model1)))
sm.density.compare(c(temp.plot$Baseline,temp.plot$Model1), group = group.index, model = "equal", col = c("black","red"), xlab = "Fantasy Points", xlim = c(140,240))
sm.options(col.band = "green")
title("Kernel Density Plot (Weeks 2-16)") #  of Fantasy Points for Lineups Generated in Weeks 2-16 (At Least 160)
legend("topright", legend = c("Baseline", "Spiked"), cex = 0.9, fill = c("black","red"))

hist(temp.plot$Baseline)
hist(temp.plot$Model1)
