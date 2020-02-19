if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Add ValueWR column to julia input files.

addValueWR <- function(offense_data, max_salary) {
  offense_data$ValueWR <- 0
  offense_data$ValueWR[offense_data$Salary <= max_salary & offense_data$Position == "WR"] <- 1
  return(offense_data$ValueWR)
}