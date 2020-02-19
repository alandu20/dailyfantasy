if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### DESCRIPTION #######
# Function that returns the PnL of a given user in contests found in baseline_contests.csv

userPnL <- function(user_name) {
  ####### Import Functions #######
  source("MLB/functions_global/parseContestStandings.R")
  source("MLB/functions_global/cleanPlayerNames.R")
  
  ###### Set Contests
  baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)
  
  ###### Make Copy to Store User Results
  temp_user_results <- baseline_contests
  
  # print username
  print(paste0("Username: ", user_name))
  
  # iterate
  for (i in 1:nrow(baseline_contests)) {
    ###### Parse and Subset
    # print(paste0(baseline_contests$Date[i], ": ", baseline_contests$Contest_names[i]))
    temp_standings <- parseContestStandings(baseline_contests$Date[i], baseline_contests$Contest_names[i])
    temp_user_standings <- temp_standings[temp_standings$User_Name==user_name, ]
    
    ###### Number of Cashing Lineups
    temp_payouts <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/", baseline_contests$Contest_names[i], "/payout_structure.csv"), stringsAsFactors = F, header = T)
    
    # store
    temp_user_results$Num_Lineups[i] <- nrow(temp_user_standings)
    temp_user_results$Num_Cashing[i] <- sum(temp_user_standings$Rank < temp_payouts$Place_hi[nrow(temp_payouts)])
    
    # init
    # temp_user_standings$Payout <- NA
    
    if (nrow(temp_user_standings) != 0) {
      ###### PnL
      for (j in 1:nrow(temp_user_standings)) {
        if (temp_user_standings$Rank[j] > temp_payouts$Place_hi[nrow(temp_payouts)]) {
          temp_user_standings$Payout[j] <- 0
        }
        else {
          for (k in 1:nrow(temp_payouts)) {
            if (temp_user_standings$Rank[j] >= temp_payouts$Place_lo[k] && temp_user_standings$Rank[j] <= temp_payouts$Place_hi[k]) {
              temp_user_standings$Payout[j] <- temp_payouts$Payout[k]
              break
            }
          }
        }
      }
      
      # get contest entry fee for PnL calculation
      contest_info <- read.csv(file = "MLB/data_warehouse/contests.csv", stringsAsFactors = F, header = T)
      temp_entry_fee <- contest_info$Entry_Fee[baseline_contests$contest_row_index[i]]
      temp_entry_fee <- as.numeric(gsub("\\$", "", temp_entry_fee))
      
      # store results
      temp_user_results$PnL[i] <- sum(temp_user_standings$Payout) - temp_entry_fee*nrow(temp_user_standings)
      if (i == 1) {
        temp_user_results$PnL_Aggregate[i] <- temp_user_results$PnL[i]
      } else {
        temp_user_results$PnL_Aggregate[i] <- temp_user_results$PnL_Aggregate[i-1] + temp_user_results$PnL[i] 
      }
      temp_user_results$Best_Place[i] <- min(temp_user_standings$Rank)
    } else {
      temp_user_results$PnL[i] <- 0
      if (i == 1) {
        temp_user_results$PnL_Aggregate[i] <- 0
      } else {
        temp_user_results$PnL_Aggregate[i] <- temp_user_results$PnL_Aggregate[i-1]
      }
      temp_user_results$Best_Place[i] <- NA
    }
    
    # print
    if (i %% 10 == 0) {
      print(paste0("Total PnL on ", baseline_contests$Date[i], ": ", temp_user_results$PnL_Aggregate[i]))
    }
  }
  
  return(temp_user_results)
}