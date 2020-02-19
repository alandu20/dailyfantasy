if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### DESCRIPTION #######



####### Import Functions #######
source("MLB/functions_global/parseContestStandings.R")
source("MLB/functions_global/cleanPlayerNames.R")

###### Function Inputs
user_name <- "fallfromgrace"
# fallfromgrace, youdacao, ChipotleAddict, SaahilSud, ehafner, petteytheft89, moklovin, papagates, Awesemo, scout326
# DraftCheat, ThatStunna (don't max enter)

###### Set Contests
baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)

###### Make Copy to Store User Results
temp_user_results <- baseline_contests

for (i in 1:nrow(baseline_contests)) {
  ###### Parse and Subset
  print(paste0(baseline_contests$Date[i], ": ", baseline_contests$Contest_names[i]))
  temp_standings <- parseContestStandings(baseline_contests$Date[i], baseline_contests$Contest_names[i])
  temp_user_standings <- temp_standings[temp_standings$User_Name==user_name, ]
  
  ###### Number of Cashing Lineups
  temp_payouts <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/", baseline_contests$Contest_names[i], "/payout_structure.csv"), stringsAsFactors = F, header = T)
  # print(paste0("Number of cashing lineups / total: ", sum(temp_user_standings$Rank < temp_payouts$Place_hi[nrow(temp_payouts)]), " / ", nrow(temp_user_standings)))
  
  # store
  temp_user_results$Num_Lineups[i] <- nrow(temp_user_standings)
  temp_user_results$Num_Cashing[i] <- sum(temp_user_standings$Rank < temp_payouts$Place_hi[nrow(temp_payouts)])
  
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
    # print(paste0("Check: ", contest_info$Contest_Name[baseline_contests$contest_row_index[i]], " = ", baseline_contests$Contest_names[i]))
    temp_entry_fee <- contest_info$Entry_Fee[baseline_contests$contest_row_index[i]]
    temp_entry_fee <- as.numeric(gsub("\\$", "", temp_entry_fee))
    
    # print(paste0("Best place and payout: ", min(temp_user_standings$Rank), ", $", max(temp_user_standings$Payout)))
    print(paste0("PnL for entire contest: ", sum(temp_user_standings$Payout) - temp_entry_fee*nrow(temp_user_standings)))
    
    # store results
    temp_user_results$PnL[i] <- sum(temp_user_standings$Payout) - temp_entry_fee*nrow(temp_user_standings)
    if (i == 1) {
      temp_user_results$PnL_Aggregate[i] <- temp_user_results$PnL[i]
    } else {
      temp_user_results$PnL_Aggregate[i] <- temp_user_results$PnL_Aggregate[i-1] + temp_user_results$PnL[i] 
    }
    temp_user_results$Best_Place[i] <- min(temp_user_standings$Rank)
  } else {
    print("User did not play this contest")
    temp_user_results$PnL[i] <- 0
    if (i == 1) {
      temp_user_results$PnL_Aggregate[i] <- 0
    } else {
      temp_user_results$PnL_Aggregate[i] <- temp_user_results$PnL_Aggregate[i-1]
    }
    temp_user_results$Best_Place[i] <- NA
  }
  
  ###### Max Position Exposures
  # subset
  temp_user_lineups <- temp_user_standings[,c("P1", "P2", "C", "1B", "2B", "3B", "SS", "OF1", "OF2", "OF3")]
  
  # init
  # temp_user_results[,c("Max_Exp_Ps", "Max_Exp_C", "Max_Exp_Bs", "Max_Exp_1B", "Max_Exp_2B", "Max_Exp_3B", "Max_Exp_SS", "Max_Exp_OFs", "Max_Exp_OF1", "Max_Exp_OF2", "Max_Exp_OF3")] <- NA
  
  # pitcher
  occurences <- sort(table(unlist(temp_user_lineups[,c("P1", "P2")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_Ps[i] <- exposure[1] # top exposure rate
  
  # catcher
  occurences <- sort(table(unlist(temp_user_lineups[,c("C")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_C[i] <- exposure[1] # top exposure rate
  
  # basemen
  occurences <- sort(table(unlist(temp_user_lineups[,c("1B", "2B", "3B")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_Bs[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c("1B")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_1B[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c("2B")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_2B[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c("3B")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_3B[i] <- exposure[1] # top exposure rate
  
  # SS
  occurences <- sort(table(unlist(temp_user_lineups[,c("SS")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_SS[i] <- exposure[1] # top exposure rate
  
  # outfielders
  occurences <- sort(table(unlist(temp_user_lineups[,c("OF1", "OF2", "OF3")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OFs[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c("OF1")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OF1[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c("OF2")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OF2[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c("OF3")])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OF3[i] <- exposure[1] # top exposure rate
  
  ###### Salary Distribution by Position
  if (nrow(temp_user_standings) != 0) {
    # read DK salaries
    temp_salaries <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/" , baseline_contests$Contest_names[i], "/DKSalaries.csv"), stringsAsFactors = F, header = T)
    temp_salaries$Name <- cleanPlayerNames(temp_salaries$Name)
    
    # make copy
    temp_user_salaries <- temp_user_lineups
    temp_user_salaries[,] <- NA
    
    # fill salaries
    for (m in 1:nrow(temp_user_salaries)) {
      for (n in 1:ncol(temp_user_salaries)) {
        match_salary <- which(temp_salaries$Name==as.character(temp_user_lineups[m,n]))
        if (length(match_salary) != 1) {
          temp_user_salaries[m,n] <- NA
        } else {
          temp_user_salaries[m,n] <- temp_salaries$Salary[which(temp_salaries$Name==as.character(temp_user_lineups[m,n]))] 
        }
      }
    }
    
    temp_user_salaries <- na.omit(temp_user_salaries)
    
    temp_user_results$Salary_Avg_Ps[i] <- mean(temp_user_salaries$P1 + temp_user_salaries$P2)
    temp_user_results$Salary_Avg_C[i] <- mean(temp_user_salaries$C)
    temp_user_results$Salary_Avg_Bs[i] <- mean(temp_user_salaries$`1B` + temp_user_salaries$`2B` + temp_user_salaries$`3B`)
    temp_user_results$Salary_Avg_SS[i] <- mean(temp_user_salaries$SS)
    temp_user_results$Salary_Avg_OFs[i] <- mean(temp_user_salaries$OF1 + temp_user_salaries$OF2 + temp_user_salaries$OF3)
  }
}

###### Total PnL
print(paste0("Total PnL over all baseline_contests: ", sum(temp_user_results$PnL)))

###### Plot Aggregate PnL
plot(as.Date(temp_user_results$Date), temp_user_results$PnL_Aggregate, type = "b", main = paste0(user_name, ": Aggregate PnL (baseline_contests)"), xlab = "Contest Date", ylab = "Aggregate PnL")

###### Plot Max Exposures
if (length(which(temp_user_results$Num_Lineups <= 25)) > 0) {
  temp_user_results <- temp_user_results[-which(temp_user_results$Num_Lineups <= 25),] 
}

plot(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_Ps, col = 1, type = "l", ylim = c(0,1.25), ylab = "Exposure", xlab = "Contest Date", main = paste0(user_name, ": Position Exposures (baseline_contests)"))
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_C, col = 2, type = "l")

# points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_Bs, col = 3, type = "l")
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_1B, col = 3, type = "l")
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_2B, col = 4, type = "l")
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_3B, col = 5, type = "l")

points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_SS, col = 6, type = "l")
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_OFs, col = 7, type = "l")

# add legend
# legend(x = "topleft", legend = c("Pitchers", "Catcher", "Basemen", "Shortstop", "Outfielders"), lwd = 1, col = 1:5, cex = 0.5)
legend(x = "topleft", legend = c("Pitchers", "Catcher", "1B","2B","3B", "Shortstop", "Outfielders"), lwd = 1, col = 1:5, cex = 0.5)

# add vertical green line when Best_Place in top 5, red line when out of top 1000 (and submitted more than 50 lineups)
for (d in 1:nrow(temp_user_results)) {
  if (temp_user_results$Best_Place[d] <= 5 & !is.na(temp_user_results$Best_Place[d])) {
    abline(v = as.Date(temp_user_results$Date[d]), lty = 2, col = "green")
  }
  
  if (temp_user_results$Best_Place[d] > 1000 & temp_user_results$Num_Lineups[d] > 50 & !is.na(temp_user_results$Best_Place[d])) {
    abline(v = as.Date(temp_user_results$Date[d]), lty = 2, col = "red")
  }
}

###### Plot Mean Salary by Position
plot(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_Ps, col = 1, type = "b", ylim = c(0,30000), ylab = "Salary", xlab = "Contest Date", main = paste0(user_name, ": Salary Distribution (baseline_contests)"))
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_C, col = 2, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_Bs, col = 3, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_SS, col = 4, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_OFs, col = 5, type = "b")

# add legend
legend(x = "topleft", legend = c("Pitchers", "Catcher", "Basemen", "Shortstop", "Outfielders"), lwd = 1, col = 1:5, cex = 0.5)

