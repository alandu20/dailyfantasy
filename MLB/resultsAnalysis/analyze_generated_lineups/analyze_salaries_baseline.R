if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### DESCRIPTION #######


####### Import Functions #######
source("MLB/functions_global/cleanPlayerNames.R")

###### Set Contests
baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)

baseline_contests <- baseline_contests[-which(baseline_contests$Date=="2017-05-02"),]
baseline_contests <- baseline_contests[-which(baseline_contests$Date=="2017-05-03"),]
baseline_contests <- baseline_contests[-which(baseline_contests$Date=="2017-05-04"),]

###### Copy
temp_user_results <- baseline_contests

for (i in 1:nrow(baseline_contests)) {
  # Read
  formulation <- "formulations.formulation3_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_0.6_covar_chg75p_exp(spike)"
  temp_user_lineups <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/" , baseline_contests$Contest_names[i], "/lineups/", formulation, ".csv"), stringsAsFactors = F, header = T, check.names = F)
  
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
  
  temp_user_results$Salary_Avg_Ps[i] <- mean(as.numeric(temp_user_salaries[,1]) + as.numeric(temp_user_salaries[,2]))
  temp_user_results$Salary_Avg_C[i] <- mean(temp_user_salaries[,3])
  temp_user_results$Salary_Avg_Bs[i] <- mean(as.numeric(temp_user_salaries[,4]) + as.numeric(temp_user_salaries[,5]) + as.numeric(temp_user_salaries[,6]))
  temp_user_results$Salary_Avg_SS[i] <- mean(as.numeric(temp_user_salaries[,7]))
  temp_user_results$Salary_Avg_OFs[i] <- mean(as.numeric(temp_user_salaries[,8]) + as.numeric(temp_user_salaries[,9]) + as.numeric(temp_user_salaries[,10]))
}


###### Plot Mean Salary by Position
plot(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_Ps, col = 1, type = "b", ylim = c(0,30000), ylab = "Salary", xlab = "Contest Date", main = "Salary Distribution (baseline_contests)")
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_C, col = 2, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_Bs, col = 3, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_SS, col = 4, type = "b")
points(as.Date(temp_user_results$Date), temp_user_results$Salary_Avg_OFs, col = 5, type = "b")

# add legend
legend(x = "topleft", legend = c("Pitchers", "Catcher", "Basemen", "Shortstop", "Outfielders"), lwd = 1, col = 1:5, cex = 0.5)


