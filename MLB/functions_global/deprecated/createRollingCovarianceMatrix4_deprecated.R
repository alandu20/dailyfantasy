if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for constructing covariance matrix. Same as createRollingCovarianceMatrix
# but with 4-player covariance rather than pair-wise covariance.


####### Function for Computing Covariance Matrix Given Start and End Date #######
createRollingCovarianceMatrix4 <- function(date.start, date.end, julia_hitter_df, min_games_pctg) {
  ####### Import Libraries #######
  require(stringr)
  
  
  ###### Import Functions #######
  source("MLB/functions_global/createHistoricalFptsMatrix.R")
  
  
  # date sequence
  dates <- seq(from = as.Date(date.start), to = as.Date(date.end), by = "day")
  
  # aggregate all past players if NULL, otherwise only players in contest
  ####### Aggregate All Player Data for Each Day #######
  # load contest info file
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
  list_all_players <- NULL
  for (d in 1:length(dates)) {
    # print(dates[d])
    
    # subset contest_info by date
    temp_contest_info <- contest_info[contest_info$Contest_Date==dates[d],]
    
    # get list of players for the day
    list_players_day <- NULL
    for (i in 1:nrow(temp_contest_info)) {
      # load
      temp_hitters <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
      temp_pitchers <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/pitchers.csv"), stringsAsFactors = F, header = T)
      
      # subset columns and append
      temp_hitters <- temp_hitters[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts")]
      temp_pitchers <- temp_pitchers[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts")]
      if (is.null(julia_hitter_df)) {
        temp_players_day <- rbind(temp_hitters, temp_pitchers) 
      } else {
        temp_players_day <- temp_hitters
      }
      
      # add date
      temp_players_day$Date <- dates[d]
      
      # remove Position, Salary, GameInfo (currently just uses first game in a double header. TODO: fix this)
      temp_players_day$Position <- NULL
      temp_players_day$Salary <- NULL
      temp_players_day$GameInfo <- NULL
      
      # append
      list_players_day <- rbind(list_players_day, temp_players_day)
    }
    
    # append to the unique rows (players) to the running list
    list_all_players <- rbind(list_all_players, unique(list_players_day))
    remove(list_players_day) # remove temp
  }
  
  
  ####### Construct Matrix of Historical Fpts #######
  # print("Constructing Historical Fpts Matrix...")
  
  # list of unique player names and their position
  temp_names <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
  if (is.null(julia_hitter_df)) {
    names_unique_players <- unique(temp_names)
  } else {
    # unique(temp_names)
    names_unique_players <- paste0(julia_hitter_df$Name, "_", julia_hitter_df$teamAbbrev) # must match order of julia_hitter_df (this line should be equivlaent to running unique(temp_names) but different order)
  }
  
  # call createHistoricalFptsMatrix function
  hist_fpts_mat <- createHistoricalFptsMatrix(name_team_vec = names_unique_players, list_all_players = list_all_players, dates_vec = dates, min_games_pctg = min_games_pctg)
  
  
  ####### Construct Covariance Matrix (4 players) #######
  
  
  # return covariance matrix and counts matrix
  return(list(cov_mat, cov_mat_counts, hist_fpts_mat))
}


# debug

# date.start <- "2017-04-02"
# date.end <- "2017-05-14"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/2017-05-14/$50.00entry_MLB$300KMother'sDaySpecial/hitters.csv"), stringsAsFactors = F, header = T)
# min_games_pctg <- NULL

# asdf <- read.csv(file = "MLB/data_warehouse/2017-04-29/$33.00entry_MLB$110KFastball(Early)/covariance_mat_chg75p_exp(spike).csv", stringsAsFactors = F, header = T, check.names = F)


