if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### DESCRIPTION #######


###### Set Contests
baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)

baseline_contests <- baseline_contests[-which(baseline_contests$Date=="2017-05-02"),]
baseline_contests <- baseline_contests[-which(baseline_contests$Date=="2017-05-03"),]
baseline_contests <- baseline_contests[-which(baseline_contests$Date=="2017-05-04"),]

###### Copy
temp_user_results <- baseline_contests

for (i in 1:nrow(baseline_contests)) {
  ###### Read
  formulation <- "formulations.formulation3_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_0.6_covar_chg75p_exp(spike)"
  temp_user_lineups <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/" , baseline_contests$Contest_names[i], "/lineups/", formulation, ".csv"), stringsAsFactors = F, header = T, check.names = F)
  
  ###### Max Position Exposures
  # pitcher
  occurences <- sort(table(unlist(temp_user_lineups[,c(1,2)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_Ps[i] <- exposure[1] # top exposure rate
  
  # catcher
  occurences <- sort(table(unlist(temp_user_lineups[,c(3)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_C[i] <- exposure[1] # top exposure rate
  
  # basemen
  occurences <- sort(table(unlist(temp_user_lineups[,c(4,5,6)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_Bs[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c(4)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_1B[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c(5)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_2B[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c(6)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_3B[i] <- exposure[1] # top exposure rate
  
  # SS
  occurences <- sort(table(unlist(temp_user_lineups[,c(7)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_SS[i] <- exposure[1] # top exposure rate
  
  # outfielders
  occurences <- sort(table(unlist(temp_user_lineups[,c(8,9,10)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OFs[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c(8)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OF1[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c(9)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OF2[i] <- exposure[1] # top exposure rate
  
  occurences <- sort(table(unlist(temp_user_lineups[,c(10)])), decreasing=T)
  exposure <- occurences / nrow(temp_user_lineups)
  temp_user_results$Max_Exp_OF3[i] <- exposure[1] # top exposure rate
}


###### Plot Max Exposures
plot(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_Ps, col = 1, type = "b", ylim = c(0,1.25), ylab = "Exposure", xlab = "Contest Date", main = formulation)
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_C, col = 2, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_Bs, col = 3, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_SS, col = 4, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Max_Exp_OFs, col = 5, type = "b")

# add legend
legend(x = "topleft", legend = c("Pitchers", "Catcher", "Basemen", "Shortstop", "Outfielders"), lwd = 1, col = 1:5, cex = 0.5)







