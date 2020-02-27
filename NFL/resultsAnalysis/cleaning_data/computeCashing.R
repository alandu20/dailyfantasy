#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we create csv's for use in other files. Run once for create.file <- "millymaker.contest"
# and run again for create.file <- "50K.contest" (seperate for memory reasons; clear workspace before running
# each time).
#
# Dependencies: cleanResults.R


####### SET PARAMETERS #######
slate.days <- "" # "thu-mon" or "sun-mon" or ""
wks.20 <- c(2:9,17) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
wks.27 <- c(11:15) # c(11:15) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
wks.3 <- c(2:9,11:15,17) # $3 entry contest [50k to 1st]
# week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1 # don't need to change this ever
week.latest <- 17

contest.name <- "millymaker.contest" # "50K.contest" or "millymaker.contest"


####### CREATE FILE WITH WEEK, MIN (CASHING), MAX FOR MILLY MAKER #######
if (contest.name=="millymaker.contest") {
  # load cleaned contest results (doesn't really need to be the cleaned versions but whatever)
  for (i in c(wks.20, wks.27)) {
    if (slate.days=="thu-mon") {
      assign(paste0("contest_1M_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
    } else if (slate.days=="sun-mon") {
      assign(paste0("contest_1M_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
    } else {
      assign(paste0("contest_1M_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
    }
    print(i)
  }
  
  # load payout structure
  for (i in c(wks.20, wks.27)) {
    if (i %in% wks.20) {
      if (slate.days=="thu-mon") {
        assign(paste0("payout_1M_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_thu-mon/$20_payout_structure_week", i, ".csv"), stringsAsFactors = F))
      } else if (slate.days=="sun-mon") {
        assign(paste0("payout_1M_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_sun-mon/$20_payout_structure_week", i, ".csv"), stringsAsFactors = F))
      } else {
        assign(paste0("payout_1M_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$20_payout_structure_week", i, ".csv"), stringsAsFactors = F))
      } 
    } else if (i %in% wks.27) {
      if (slate.days=="thu-mon") {
        assign(paste0("payout_1M_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_thu-mon/$27_payout_structure_week", i, ".csv"), stringsAsFactors = F))
      } else if (slate.days=="sun-mon") {
        assign(paste0("payout_1M_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_sun-mon/$27_payout_structure_week", i, ".csv"), stringsAsFactors = F))
      } else {
        assign(paste0("payout_1M_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$27_payout_structure_week", i, ".csv"), stringsAsFactors = F))
      }
    }
  }
  
  # build matrix with week, min (cashing), max for the contest
  contest_1M_cashing <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 3, dimnames = list(NULL, c("Week","Min","Max"))))
  for (i in 1:week.latest) {
    contest_1M_cashing[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.payout <- eval(parse(text=paste0("payout_1M_structure_wk", i)))
      place.last <- temp.payout[nrow(temp.payout),'Place_hi']
      contest_1M_cashing[i,2] <- min(temp.results$Points[temp.results$Rank <= place.last])
      contest_1M_cashing[i,3] <- max(temp.results$Points[temp.results$Rank <= place.last])
      # print(i)
      # print(length(temp.results$Points))
      # print(length(temp.results$Points[temp.results$Rank <= place.last]))
    }
  }
  write.csv(contest_1M_cashing, file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/contest_1M_cashing.csv"), row.names = F)
}


####### CREATE FILE WITH WEEK, MIN (CASHING), MAX FOR [50K TO 1ST] PLAY ACTION ($3 ENTRY) CONTEST #######
if (contest.name=="50K.contest") {
  # load cleaned contest results
  for (i in wks.3) {
    if (slate.days=="thu-mon") {
      assign(paste0("contest_50K_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/$3_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else if (slate.days=="sun-mon") {
      assign(paste0("contest_50K_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/$3_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    } else {
      assign(paste0("contest_50K_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$3_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    }
    print(i)
  }

  # load payout structure
  for (i in wks.3) {
    if (slate.days=="thu-mon") {
      assign(paste0("payout_50K_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_thu-mon/$3_payout_structure_week", i, ".csv"), stringsAsFactors = F))
    } else if (slate.days=="sun-mon") {
      assign(paste0("payout_50K_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_sun-mon/$3_payout_structure_week", i, ".csv"), stringsAsFactors = F))
    } else {
      assign(paste0("payout_50K_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$3_payout_structure_week", i, ".csv"), stringsAsFactors = F))
    }
  }
  
  # build matrix with week, min (cashing), max for the contest
  contest_50K_cashing <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 3, dimnames = list(NULL, c("Week","Min","Max"))))
  for (i in 1:week.latest) {
    contest_50K_cashing[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      temp.results <- eval(parse(text=paste0("contest_50K_results_wk", i)))
      temp.payout <- eval(parse(text=paste0("payout_50K_structure_wk", i)))
      place.last <- temp.payout[nrow(temp.payout),'Place_hi']
      contest_50K_cashing[i,2] <- min(temp.results$Points[temp.results$Rank <= place.last])
      contest_50K_cashing[i,3] <- max(temp.results$Points[temp.results$Rank <= place.last])
      # print(i)
      # print(length(temp.results$Points))
      # print(length(temp.results$Points[temp.results$Rank <= place.last]))
    }
  }
  write.csv(contest_50K_cashing, file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/contest_50K_cashing.csv"), row.names = F)
}

