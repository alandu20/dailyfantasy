if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create covariance matrices for any given start and end date.


####### Import Functions #######
source("MLB/functions_global/createRollingCovarianceMatrix.R")


####### Construct Covariance and Counts Matrix and Write to CSV file #######
# dates <- seq(from = Sys.Date()-2, to = Sys.Date()-2, by = "day") # one date
dates <- seq(from = as.Date("2017-04-22"), to = as.Date("2017-04-22"), by = "day") # one date
# dates <- seq(from = as.Date("2017-04-04"), to = Sys.Date()-3, by = "day") # range of dates

for (i in 1:length(dates)) {
  # end date in covariance matrix function
  date_last <- dates[i]

  # construct covariance and counts matrices
  cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = NULL, filter_on = F)
  cov_mat <- cov.dat[[1]]
  cov_mat_counts <- cov.dat[[2]]

  # set NAs to 0 in covariance matrix for julia code
  cov_mat[is.na(cov_mat)] <- 0
  cov_mat_counts[is.na(cov_mat_counts)] <- 0

  # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
  write.csv(cov_mat, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_mat.csv"), row.names = F)
  write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_counts_mat.csv"), row.names = F)

  print(date_last+1)
}


####### Create Covariance Matrices for a Given Day's Contests #######
# loop through days if desired
dates_last <- seq(from = Sys.Date() - 2, to = Sys.Date() - 2, by = "day") # one date
# dates_last <- seq(from = as.Date("2017-04-04"), to = Sys.Date() - 2, by = "day") # range of dates
for (d in 1:length(dates_last)) {
  # load contest info file
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

  # subset by date
  date_last <- dates_last[d] # end date in covariance matrix function
  print(date_last)
  contest_info <- contest_info[contest_info$Contest_Date==(as.Date(date_last)+1),]

  # identify contests that have the same julia input file so that we don't need to run the covariance code multiple times for the same set of players
  contest_info$Match_ID <- NA
  list_contest <- NULL
  temp_ind <- 1
  for (i in 1:nrow(contest_info)) {
    # load julia input file
    temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)

    # if no other hitter dfs in list then add first df to list
    if (is.null(list_contest)==TRUE) {
      list_contest[[1]] <- temp_julia_hitter_df
      contest_info$Match_ID[i] <- temp_ind # add new match index
      temp_ind <- temp_ind + 1 # increment match index
    } else {
      # check each df in list for matches
      for (j in 1:length(list_contest)) {
        # if df matches previous df then don't add it
        if (sum(!(list_contest[[j]]$Name %in%temp_julia_hitter_df$Name)) == 0) {
          contest_info$Match_ID[i] <- j # match the index in the list of df
          break
        }
      }

      # if there was no match in the df (i.e. Match_ID[i] is NA) then add df to list
      if (is.na(contest_info$Match_ID[i])==TRUE) {
        contest_info$Match_ID[i] <- temp_ind # add new match index
        temp_ind <- temp_ind + 1 # increment match index
        list_contest[[length(list_contest)+1]] <- temp_julia_hitter_df
      }
    }
  }
  print(paste0("Number of contests with unique hitter.csv files: ", unique(contest_info$Match_ID)))


  # iterate through the contests (only call createRollingCovarianceMatrix when contest's corresponding Match_ID hasn't been run yet)
  for (i in 1:nrow(contest_info)) {
    # read in julia input file for this date
    temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)

    print(paste0("Begin (Contest ", i, " / ", nrow(contest_info),"): ", date_last, " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i]))))

    # if contest's hitters corresponding covar mat hasn't been computed already, then do so. else, use an existing covar mat
    if (contest_info$Match_ID[i] %in% contest_info$Match_ID[1:(i-1)] == FALSE | i==1) {
      # construct covariance and counts matrices
      cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = temp_julia_hitter_df, filter_on = F)
      cov_mat <- cov.dat[[1]]
      cov_mat_counts <- cov.dat[[2]]

      # set NAs to 0 in covariance matrix for julia code
      cov_mat[is.na(cov_mat)] <- 0
      cov_mat_counts[is.na(cov_mat_counts)] <- 0

      # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
      write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat.csv"), row.names = F)
      write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat.csv"), row.names = F)
    } else {
      ind_match <- min(which(contest_info$Match_ID[1:i] %in% contest_info$Match_ID[i]))

      cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/covariance_mat.csv"), stringsAsFactors = F, header = T, check.names=FALSE)
      cov_mat_counts <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/covariance_counts_mat.csv"), stringsAsFactors = F, header = T, check.names=FALSE)

      # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
      write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat.csv"), row.names = F)
      write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat.csv"), row.names = F)
    }

    print(paste0("Completed: ", date_last, " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), ": ", nrow(temp_julia_hitter_df), "=", nrow(cov_mat)))
  }
}


