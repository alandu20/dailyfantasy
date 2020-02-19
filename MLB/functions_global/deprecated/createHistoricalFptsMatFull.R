if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for constructing the hist_fpts_mat.csv. Part of createRollingCovarianceMatrix() function.


####### Begin function
createHistoricalFptsMatFull <- function(date.start, date.end, julia_hitter_df, min_games_pctg) {
  ####### Import Libraries #######
  require(stringr)
  
  
  ###### Import Functions #######
  source("MLB/functions_global/createHistoricalFptsMatrix.R")
  source("MLB/functions_global/aggregateAllPlayerResults.R")
  source("MLB/functions_global/listMissingDates.R")
  
  
  # date sequence
  dates <- seq(from = as.Date(date.start), to = as.Date(date.end), by = "day")
  dates <- dates[-which(dates %in% listMissingDates())]
  
  
  ####### Aggregate All Player Data for Each Day #######
  list_all_players <- aggregateAllPlayerResults(dates, julia_hitter_df)
  
  
  ####### Construct Matrix of Historical Fpts #######
  # print("Constructing Historical Fpts Matrix...")
  
  # list of unique player names and their position
  temp_names <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
  if (is.null(julia_hitter_df)) {
    names_unique_players <- unique(temp_names)
  } else {
    names_unique_players <- paste0(julia_hitter_df$Name, "_", julia_hitter_df$teamAbbrev) # must match order of julia_hitter_df (this line should be equivlaent to running unique(temp_names) but different order)
    if (length(names_unique_players) != length(unique(names_unique_players))) {
      print("========== PROBLEM: SOMETHING WEIRD IN DKSALARIES.CSV ==========")
      print("For some reason there are duplicate players in DKSalaries.csv and hitters.csv. Examine files.")
      stop("See above. Fix and rerun.")
      names_unique_players <- unique(names_unique_players)
    }
  }
  
  # call createHistoricalFptsMatrix function
  hist_fpts_mat <- createHistoricalFptsMatrix(name_team_vec = names_unique_players, list_all_players = list_all_players, dates_vec = dates, min_games_pctg = min_games_pctg)
  
  
  # return covariance matrix and counts matrix
  return(hist_fpts_mat)
}

















####### Set Dates (T+1)
date.start = "2017-07-18"
date.end = "2017-07-20"



####### Section II (hist_fpts_mat.csv matrix - full) #######
print("Creating full hist_fpts_mat.csv matrix...")

dates_last <- seq(from = as.Date(date.start) - 2, to  = as.Date(date.end) - 2, by = "day") # date range
for (i in 1:length(dates_last)) {
  # end date in covariance matrix function
  date_last <- dates_last[i]
  
  # construct covariance and counts matrices
  hist_fpts_mat <- createHistoricalFptsMatFull(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = NULL, min_games_pctg = 0.05)
  
  
  # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
  write.csv(hist_fpts_mat, file = paste0("MLB/data_warehouse/", date_last+1, "/hist_fpts_mat.csv"), row.names = T)
  
  print(paste0(date_last+1, " full covariance matrix completed"))
}