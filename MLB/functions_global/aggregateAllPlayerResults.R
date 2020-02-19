if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

####### Description #######
# Function for constructing dataframe where rows are all player's results for a given day.

aggregateAllPlayerResults <- function(dates, julia_hitter_df) {
  ####### Import Functions #######
  source("MLB/functions_global/listMissingDates.R")
  
  
  ####### Aggregate All Player Data for Each Day #######
  # initializations
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
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
        temp.dksalaries.path <- paste0(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/DKSalaries.csv"))
        if (file.exists(temp.dksalaries.path)) {
          # load
          temp_hitters <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
          temp_pitchers <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/pitchers.csv"), stringsAsFactors = F, header = T)
          
          # subset columns and append
          temp_hitters <- temp_hitters[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts", "Batting_Order_Confirmed")]
          temp_pitchers <- temp_pitchers[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts")]
          temp_pitchers$Batting_Order_Confirmed <- NA # empty column for appending hitters and pitchers
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
        } else {
          # only print missing contest folders for last date
          if (d==length(dates)) {
            print(paste0("contest folder missing ", dates[d], ": ", temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i]))) 
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