#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we clean resultsAnalysis/data_warehouse/contest_results files.


####### SET SECTION TO RUN #######
run.section <- 1 # 1 if milly maker, 2 if $3 entry play action


####### SET PARAMETERS #######
slate.days <- "" # "thu-mon" or "sun-mon" or ""
wks.20 <- c(2:9,17) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
wks.27 <- c(11:15) # c(11:15) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
wks.3 <- c(2:9,11:15,17) # $3 entry contest [50k to 1st]
wks.4 <- c(10,16)


####### IMPORT LIBRARIES #########
library('stringr')


###### LOAD ######
if (run.section == 2) {
  for (i in c(wks.3, wks.4)) {
    if (i %in% wks.3) {
      entry.fee <- "$3"
    } else if (i %in% wks.4) {
      entry.fee <- "$4"
    }
    name <- paste0("contest_playaction_results_wk", i)
    if (slate.days=="thu-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/", entry.fee, "_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else if (slate.days=="sun-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/", entry.fee, "_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", entry.fee, "_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    }
    print(i)
  }
} else if (run.section == 1) {
  # load $20 contest results
  for (i in wks.20) {
    name <- paste0("contest_1M_results_wk", i)
    if (slate.days=="thu-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/$20_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else if (slate.days=="sun-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/$20_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$20_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    }
    print(i)
  }
  
  # load $27 contest results
  for (i in wks.27) {
    name <- paste0("contest_1M_results_wk", i)
    if (slate.days=="thu-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/$27_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else if (slate.days=="sun-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/$27_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$27_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    }
    print(i)
  }
}


###### FUNCTIONS ######
splitPlayers <- function(x) {
  return(str_split_fixed(x, "QB | RB | WR | TE | FLEX | DST ", 10)[2:10])
}


####### CLEAN MILLY MAKER CONTEST RESULTS (ONLY NEED TO RUN FOR NEW WEEKS - HARD CODED) #######
if (run.section == 1) {
  for (i in c(wks.20, wks.27)) {
    temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
    temp.results$TimeRemaining <- NULL
    temp.results$X <- NULL
    temp.results$Player <- NULL
    temp.results$X.Drafted <- NULL
    temp.results$FPTS <- NULL # only exists in weeks 9,11:15,17
    temp.ind <- ncol(temp.results)
    temp.results[,(temp.ind+1):(temp.ind+9)] <- NA
    colnames(temp.results)[(temp.ind+1):(temp.ind+9)] <- c('QB','RB1','RB2','WR1','WR2','WR3','TE','FLEX','DST')
    
    temp.players <- data.frame(matrix(unlist(lapply(X = temp.results$Lineup[1:nrow(temp.results)], FUN = splitPlayers)), nrow=nrow(temp.results), byrow=T))
    temp.results[,(temp.ind+1):(temp.ind+9)] <- temp.players
    temp.results$DST <- sub(" ", "", temp.results$DST)
    
    temp.entry <- str_split_fixed(temp.results$EntryName, " ", 2) # split at @ symbol
    temp.results$User.Name <- temp.entry[,1]
    temp.results$Entry.Num <- temp.entry[,2]
    
    if (slate.days=="thu-mon") {
      write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/cleaned/1M_contest_full_results_week"),i,".csv", row.names = F)
    } else if (slate.days=="sun-mon") {
      write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/cleaned/1M_contest_full_results_week"),i,".csv", row.names = F)
    } else {
      write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/1M_contest_full_results_week",i,".csv"), row.names = F)
    }
    
    print(i)
  }
  
  # for (i in c(9,11:15,17)) {
  #   temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
  #   temp.results$FPTS <- NULL
  #   write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/1M_contest_full_results_week",i,".csv"), row.names = F)
  # } 
}



####### CLEAN PLAY ACTION ($3 OR $4) CONTEST RESULTS #######
if (run.section == 2) {
  for (i in c(wks.3, wks.4)) {
    temp.results <- eval(parse(text=paste0("contest_playaction_results_wk", i)))
    temp.results$TimeRemaining <- NULL
    temp.results$X <- NULL
    temp.results$Player <- NULL
    temp.results$X.Drafted <- NULL
    temp.results$FPTS <- NULL # exists after week 7
    temp.ind <- ncol(temp.results)
    temp.results[,(temp.ind+1):(temp.ind+9)] <- NA
    colnames(temp.results)[(temp.ind+1):(temp.ind+9)] <- c('QB','RB1','RB2','WR1','WR2','WR3','TE','FLEX','DST')

    temp.players <- data.frame(matrix(unlist(lapply(X = temp.results$Lineup[1:nrow(temp.results)], FUN = splitPlayers)), nrow=nrow(temp.results), byrow=T))
    temp.results[,(temp.ind+1):(temp.ind+9)] <- temp.players
    temp.results$DST <- sub(" ", "", temp.results$DST)

    temp.entry <- str_split_fixed(temp.results$EntryName, " ", 2) # split at @ symbol
    temp.results$User.Name <- temp.entry[,1]
    temp.results$Entry.Num <- temp.entry[,2]

    if (slate.days=="thu-mon") {
      write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/cleaned/playaction_contest_full_results_week"),i,".csv", row.names = F)
    } else if (slate.days=="sun-mon") {
      write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/cleaned/playaction_contest_full_results_week"),i,".csv", row.names = F)
    } else {
      write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/playaction_contest_full_results_week",i,".csv"), row.names = F)
    }

    print(i)
  }
}






