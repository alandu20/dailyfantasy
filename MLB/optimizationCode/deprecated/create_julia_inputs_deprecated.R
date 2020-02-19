if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create julia inputs (aggregated projections for hitters and pitchers) for each date and contest.


####### Import Functions #######
source("MLB/functions_global/aggregateJuliaDF.R")


####### Create Julia Inputs #######
# dates <- seq(from = as.Date("2017-04-02"), to = as.Date("2017-04-02"), by = "day") # one date
dates <- seq(from = as.Date("2017-04-02"), to = as.Date("2017-04-25"), by = "day") # range of dates
# dates <- seq(from = as.Date("2017-04-02"), to = Sys.Date() - 1, by = "day") # typical range of dates
for (d in 1:length(dates)) {
  # load full contest info file
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
  
  # subset by yesterday's date
  date <- dates[d]
  contest_info <- contest_info[contest_info$Contest_Date==as.Date(date),]
  
  # aggregate
  aggregated_data_hitters <- list()
  aggregated_data_pitchers <- list()
  for (i in 1:nrow(contest_info)) {
    projections.dat <- aggregateJuliaDF(contest.date = contest_info$Contest_Date[i], contest.name = paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])))
    aggregated_data_hitters[[i]] <- projections.dat[[1]]
    aggregated_data_pitchers[[i]] <- projections.dat[[2]]
    
    # remove NAs in pitchers df
    aggregated_data_pitchers[[i]] <- aggregated_data_pitchers[[i]][!is.na(aggregated_data_pitchers[[i]]$Projection_dfn),]
    
    ####### Write to CSV file #######
    write.csv(aggregated_data_hitters[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), row.names = F)
    write.csv(aggregated_data_pitchers[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/pitchers.csv"), row.names = F)
  }
  
  print(dates[d])
}


