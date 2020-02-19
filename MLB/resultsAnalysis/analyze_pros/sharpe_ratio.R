if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Comparing users based on total PnL alone is not sufficient. For example, a user that achieves a PnL of 100,000 with standard deviation 10,000 is arguably inferior to a user that achieves a PnL of 90,000 with standard deviation 2,500. One measure to account for PnL volatility is the Sharpe ratio, which is used to calculate risk adjusted returns. The formula used in modern portfolio theory is Sharpe ratio = (Mean portfolio return - Risk-free rate)/Standard deviation of portfolio return. We simplify the measure into Sharpe ratio = Total PnL / Standard deviation Given that our lineups suffer heavy losses on days where none take 1st place, we are interested in studying the lineups of users that achieve the highest Sharpe ratios (high PnL, low standard deviation).

# load functions
source("MLB/resultsAnalysis/helperFunctions/userPnL.R")

# list of pros to study
list_users <- c("fallfromgrace", "youdacao", "ChipotleAddict", "SaahilSud", "ehafner", "petteytheft89", "moklovin", "papagates", "Awesemo", "scout326", "DraftCheat", "ThatStunna")

# initialize
sharpe.df <- as.data.frame(matrix(data = NA, nrow = length(list_users), ncol = 8, dimnames = list(NULL, c("Username", "Num_Contests", "Total_PnL", "SD", "Sharpe_Ratio", "Max_Drawdown", "Neg_Avg", "Neg_SD"))))

for (i in 1:length(list_users)) {
  # compute PnL for user
  temp_user <- list_users[i]
  temp_user_pnl <- userPnL(user_name = temp_user)
  
  # store results df
  assign(paste0("results_", temp_user), temp_user_pnl)
  
  # remove contests < 10 lineups
  if (length(which(temp_user_pnl$Num_Lineups <= 10)) > 0) {
    temp_user_pnl <- temp_user_pnl[-which(temp_user_pnl$Num_Lineups <= 10), ]
  }
  
  # add to df
  sharpe.df$Username[i] <- temp_user
  sharpe.df$Num_Contests[i] <- nrow(temp_user_pnl)
  sharpe.df$Total_PnL[i] <- sum(temp_user_pnl$PnL)
  sharpe.df$SD[i] <- sd(temp_user_pnl$PnL)
  sharpe.df$Sharpe_Ratio[i] <- sum(temp_user_pnl$PnL) / sd(temp_user_pnl$PnL)
  sharpe.df$Max_Drawdown[i] <- min(temp_user_pnl$PnL_Aggregate)
  sharpe.df$Neg_Avg[i] <- mean(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)])
  sharpe.df$Neg_SD[i] <- sd(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)])
  sharpe.df$Neg_PnLperLineup_Avg[i] <- mean(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)] / temp_user_pnl$Num_Lineups[which(temp_user_pnl$PnL < 0)])
  sharpe.df$Neg_PnLperLineup_SD[i] <- sd(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)] / temp_user_pnl$Num_Lineups[which(temp_user_pnl$PnL < 0)])
  
  # print
  print(paste0("Num Contests: ", nrow(temp_user_pnl)))
  print(paste0("Total PnL: ", sum(temp_user_pnl$PnL)))
  print(paste0("SD: ", sd(temp_user_pnl$PnL)))
  print(paste0("Sharpe Ratio: ", sum(temp_user_pnl$PnL) / sd(temp_user_pnl$PnL)))
  print(paste0("Max Drawdown: ", min(temp_user_pnl$PnL_Aggregate)))
  print(paste0("Neg Days Avg: ", mean(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)])))
  print(paste0("Neg Days SD: ", sd(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)])))
  print(paste0("Neg Days PnL/Lineup Avg: ", mean(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)] / temp_user_pnl$Num_Lineups[which(temp_user_pnl$PnL < 0)])))
  print(paste0("Neg Days PnL/Lineup SD: ", sd(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)] / temp_user_pnl$Num_Lineups[which(temp_user_pnl$PnL < 0)])))
  cat("\n")
}

# save workspace variables
# save(list = ls(all.names = TRUE), file = "MLB/resultsAnalysis/analyze_pros/sharpe_ratio.RData", envir = .GlobalEnv)
# load("MLB/resultsAnalysis/analyze_pros/sharpe_ratio.RData")

# study youdacao, fallfromgrace, Awesemo

# debug
# for (i in 1:length(list_users)) {
#   temp_user_pnl <- eval(parse(text=paste0("results_", list_users[i])))
#   
#   if (length(which(temp_user_pnl$Num_Lineups <= 10)) > 0) {
#     temp_user_pnl <- temp_user_pnl[-which(temp_user_pnl$Num_Lineups <= 10), ]
#   }
#   
#   sharpe.df$Neg_PnLperLineup_Avg[i] <- mean(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)] / temp_user_pnl$Num_Lineups[which(temp_user_pnl$PnL < 0)])
#   sharpe.df$Neg_PnLperLineup_SD[i] <- sd(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)] / temp_user_pnl$Num_Lineups[which(temp_user_pnl$PnL < 0)])
#   
#   sharpe.df$Neg_Avg[i] <- mean(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)])
#   sharpe.df$Neg_SD[i] <- sd(temp_user_pnl$PnL[which(temp_user_pnl$PnL < 0)])
# }


