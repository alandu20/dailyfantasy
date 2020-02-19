if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

####### Description #######
# Function for constructing dataframe where rows are all player's results for a given day.

aggregateAllPlayerResults <- function(dates, julia_df) {
  ####### Import Functions #######
  source("NBA/functions_global/listMissingDates.R")
  
  
  ####### Aggregate All Player Data for Each Day #######
  # initializations
  contest_info <- read.csv(file = 'NBA/data_warehouse/contests.csv', stringsAsFactors = F)
  list_all_players <- NULL
  missing_dates <- listMissingDates()
  
  # iterate through all dates
  for (d in 1:length(dates)) {
    # skip if no data for this date
    if (!(dates[d] %in% missing_dates)) {
      # subset contest_info by date
      temp_contest_info <- contest_info[contest_info$Contest_Date==dates[d],]
      
      # get list of players for the day
      list_players_day <- NULL
      for (i in 1:nrow(temp_contest_info)) {
        # check if contest folder exists
        temp.dksalaries.path <- paste0(file = paste0("NBA/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",temp_contest_info$Contest_Name[i]), "/DKSalaries.csv"))
        if (file.exists(temp.dksalaries.path)) {
          # load
          temp <- read.csv(file = paste0("NBA/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",temp_contest_info$Contest_Name[i]), "/players.csv"), stringsAsFactors = F, header = T)
          
          # subset columns and append
          temp <- temp[, c("Position", "Name", "Salary", "GameInfo", "Team", "Actual_fpts")]
          temp_players_day <- temp
          
          # add date
          temp_players_day$Date <- dates[d]
          
          # remove Position, Salary, GameInfo (currently just uses first game in a double header. TODO: fix this)
          temp_players_day$Position <- NULL
          temp_players_day$Salary <- NULL
          temp_players_day$GameInfo <- NULL
          
          # append
          list_players_day <- rbind(list_players_day, temp_players_day)
        } else {
          # only print missing contest folders for last date
          if (d==length(dates)) {
            print(paste0("contest folder missing ", dates[d], ": ", temp_contest_info$Entry_Fee[i],"entry_",temp_contest_info$Contest_Name[i]))
          }
        }
      }
      
      # append to the unique rows (players) to the running list
      list_all_players <- rbind(list_all_players, unique(list_players_day))
      remove(list_players_day) # remove temp 
    }
  }
  
  return(list_all_players)
}