# TODO:

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create julia inputs for each date and contest:
# - Section I: aggregated projections, actual fpts for offense and defense for each contest

NBA_create_julia_inputs <- function(date_start, date_end) {
  ####### Import Functions #######
  source("NBA/functions_global/aggregateJuliaDF.R")
  
  ####### Section I (player data df) #######
  print("Creating Player Input Data Dataframe...")
  
  dates <- seq(from = as.Date(date_start), to = as.Date(date_end), by = "day") # one date
  for (d in 1:length(dates)) {
    # load full contest info file
    contest_info <- read.csv(file = 'NBA/data_warehouse/contests.csv', stringsAsFactors = F)
    
    # subset by date
    date <- dates[d]
    contest_info <- contest_info[contest_info$Contest_Date==as.Date(date),]
    
    # aggregate
    aggregated_data <- list()
    for (i in 1:nrow(contest_info)) {
      # check if folder for contest exists
      temp.dksalaries.path <- paste0(file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/", contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i], "/DKSalaries.csv"))
      if (file.exists(temp.dksalaries.path)) {
        aggregated_data[[i]] <- aggregateJuliaDF(contest.date = contest_info$Contest_Date[i], contest.name = paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]))
        
        # set NAs to 0
        aggregated_data[[i]][is.na(aggregated_data[[i]])] <- 0
        
        ####### Write to CSV file #######
        write.csv(aggregated_data[[i]], file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]), "/players.csv"), row.names = F)
      } else {
        print(paste0("contest folder missing: ", paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/", paste0(contest_info$Entry_Fee[i],"entry_", contest_info$Contest_Name[i]))))
      }
    }
    
    print(paste0(dates[d], " completed"))
  }
}


NBA_create_julia_inputs(date_start = "2017-12-26", date_end = "2018-01-01")


