if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create julia inputs for each date and contest:
# - Section I: aggregated projections, actual fpts, projected BO, confirmed BO for hitters and pitchers for each contest
# - Section II: hitters covariance matrices (full)
# - Section III: hitters covariance matrices for each contest


####### Import Functions #######
source("MLB/functions_global/aggregateJuliaDF.R")
source("MLB/functions_global/createRollingCovarianceMatrix.R")
source("MLB/functions_global/filterCovarianceMatrix.R")


####### Import Functions #######
date.start <-  Sys.Date() # Sys.Date() # "2017-04-07"
date.end <- Sys.Date() # Sys.Date() # "2017-04-29"


####### Section I (player data df) #######
print("Creating Player Input Data Dataframe...")

dates <- seq(from = as.Date(date.start)-1, to = as.Date(date.end)-1, by = "day") # one date
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
    
    # DK changed team name convention to all caps starting 4/29, so we convert everything to all caps
    # TODO (done in aggregateJuliaDF.R file for now)
    
    ####### Write to CSV file #######
    write.csv(aggregated_data_hitters[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), row.names = F)
    write.csv(aggregated_data_pitchers[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/pitchers.csv"), row.names = F)
  }
  
  print(dates[d])
}


####### Section II (covariance matrices - full) #######
print("Creating full covariance matrices...")

dates_last <- seq(from = as.Date(date.start) - 2, to  = as.Date(date.end) - 2, by = "day") # date range
for (i in 1:length(dates_last)) {
  # end date in covariance matrix function
  date_last <- dates_last[i]
  
  # construct covariance and counts matrices
  cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = NULL, min_games_pctg = 0.2)
  cov_mat <- cov.dat[[1]]
  cov_mat_counts <- cov.dat[[2]]
  
  # set NAs to 0 in covariance matrix for julia code
  cov_mat[is.na(cov_mat)] <- 0
  cov_mat_counts[is.na(cov_mat_counts)] <- 0
  
  # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
  write.csv(cov_mat, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_mat.csv"), row.names = F)
  write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_counts_mat.csv"), row.names = F)
  
  print(paste0(date_last+1, " full covariance matrix completed"))
}


####### Section III (covariance matrices - contest) #######
# print("Creating contest covariance matrices...")
# 
# dates_last <- seq(from = as.Date(date.start) - 2, to  = as.Date(date.end) - 2, by = "day") # date range
# for (d in 25:length(dates_last)) {
#   # load contest info file
#   contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
# 
#   # subset by date
#   date_last <- dates_last[d] # end date in covariance matrix function
#   contest_info <- contest_info[contest_info$Contest_Date==(as.Date(date_last)+1),] # +1 to avoid look ahead bias
#   print(paste0("End Date in createRollingCovarianceMatrix function: ", contest_info$Contest_Date[1]))
# 
#   # identify contests that have the same julia input file so that we don't need to run the covariance code multiple times for the same set of players
#   contest_info$Match_ID <- NA
#   list_contest <- NULL
#   temp_ind <- 1
#   for (i in 1:nrow(contest_info)) {
#     # load julia input file
#     temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
# 
#     # if no other hitter dfs in list then add first df to list
#     if (is.null(list_contest)==TRUE) {
#       list_contest[[1]] <- temp_julia_hitter_df
#       contest_info$Match_ID[i] <- temp_ind # add new match index
#       temp_ind <- temp_ind + 1 # increment match index
#     } else {
#       # check each df in list for matches
#       for (j in 1:length(list_contest)) {
#         # if df matches previous df then don't add it
#         if (identical(list_contest[[j]]$Name, temp_julia_hitter_df$Name)==T) {
#           contest_info$Match_ID[i] <- j # match the index in the list of df
#           break
#         }
#       }
# 
#       # if there was no match in the df (i.e. Match_ID[i] is NA) then add df to list
#       if (is.na(contest_info$Match_ID[i])==TRUE) {
#         contest_info$Match_ID[i] <- temp_ind # add new match index
#         temp_ind <- temp_ind + 1 # increment match index
#         list_contest[[length(list_contest)+1]] <- temp_julia_hitter_df
#       }
#     }
#   }
#   print(paste0("Number of contests with unique hitter.csv files: ", length(unique(contest_info$Match_ID))))
# 
# 
#   # iterate through the contests (only call createRollingCovarianceMatrix when contest's corresponding Match_ID hasn't been run yet)
#   for (i in 1:nrow(contest_info)) {
#     # read in julia input file for this date
#     temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
# 
#     print(paste0("Begin (Contest ", i, " / ", nrow(contest_info),"): ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i]))))
# 
#     # if contest's hitters corresponding covar mat hasn't been computed already, then do so. else, use an existing covar mat
#     if (contest_info$Match_ID[i] %in% contest_info$Match_ID[1:(i-1)] == FALSE | i==1) {
#       print("Constructing covariance matrix for this contest...")
# 
#       # construct covariance and counts matrices
#       use_filter_name <- "chg75p_zeros" # test, chg75p_spike, chg75p_exp(spike), chg75p_zeros
#       cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = temp_julia_hitter_df, filter_name = use_filter_name)
#       cov_mat <- cov.dat[[1]]
#       cov_mat_counts <- cov.dat[[2]]
# 
#       # set NAs to 0 in covariance matrix for julia code
#       cov_mat[is.na(cov_mat)] <- 0
#       cov_mat_counts[is.na(cov_mat_counts)] <- 0
# 
#       # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
#       # write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat.csv"), row.names = F)
#       # write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat.csv"), row.names = F)
# 
#       write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat_", use_filter_name, ".csv"), row.names = F)
# 
#     } else {
#       ind_match <- min(which(contest_info$Match_ID[1:i] %in% contest_info$Match_ID[i]))
# 
#       # cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/covariance_mat.csv"), stringsAsFactors = F, header = T, check.names=FALSE)
#       # cov_mat_counts <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/covariance_counts_mat.csv"), stringsAsFactors = F, header = T, check.names=FALSE)
# 
#       cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/covariance_mat_", use_filter_name, ".csv"), stringsAsFactors = F, header = T, check.names=FALSE)
# 
#       # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
#       # write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat.csv"), row.names = F)
#       # write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat.csv"), row.names = F)
# 
#       write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat_", use_filter_name, ".csv"), row.names = F)
#     }
# 
#     print(paste0("Completed: ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), ": ", nrow(temp_julia_hitter_df), "=", nrow(cov_mat)))
#   }
# }


####### Section III (covariance matrices - contest - no filtering) #######
print("Creating contest covariance matrices (no filtered)...")

dates_last <- seq(from = as.Date(date.start) - 2, to  = as.Date(date.end) - 2, by = "day") # date range
for (d in 1:length(dates_last)) {
  # load contest info file
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
  
  # subset by date
  date_last <- dates_last[d] # end date in covariance matrix function
  contest_info <- contest_info[contest_info$Contest_Date==(as.Date(date_last)+1),] # +1 to avoid look ahead bias
  print(paste0("End Date in createRollingCovarianceMatrix function: ", contest_info$Contest_Date[1]))
  
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
        if (identical(list_contest[[j]]$Name, temp_julia_hitter_df$Name)==T) {
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
  print(paste0("Number of contests with unique hitter.csv files: ", length(unique(contest_info$Match_ID))))
  
  
  # iterate through the contests (only call createRollingCovarianceMatrix when contest's corresponding Match_ID hasn't been run yet)
  for (i in 1:nrow(contest_info)) {
    # read in julia input file for this date
    temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
    
    print(paste0("Begin (Contest ", i, " / ", nrow(contest_info),"): ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i]))))
    
    # if contest's hitters corresponding covar mat hasn't been computed already, then do so. else, use an existing covar mat
    if (contest_info$Match_ID[i] %in% contest_info$Match_ID[1:(i-1)] == FALSE | i==1) {
      print("Constructing covariance matrix for this contest...")
      
      # construct covariance and counts matrices
      cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = temp_julia_hitter_df, min_games_pctg = NULL)
      cov_mat <- cov.dat[[1]]
      cov_mat_counts <- cov.dat[[2]]
      hist_fpts_mat <- cov.dat[[3]]
      
      # set NAs to 0 in covariance matrix for julia code
      cov_mat[is.na(cov_mat)] <- 0
      cov_mat_counts[is.na(cov_mat_counts)] <- 0
      
      # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
      write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat_unfiltered.csv"), row.names = F)
      write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat_unfiltered.csv"), row.names = F)
      write.csv(hist_fpts_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hist_fpts_mat.csv"), row.names = T) # row.names = T to output the player names
      
    } else {
      ind_match <- min(which(contest_info$Match_ID[1:i] %in% contest_info$Match_ID[i]))
      
      cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/covariance_mat_unfiltered.csv"), stringsAsFactors = F, header = T, check.names=FALSE)
      cov_mat_counts <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/covariance_counts_mat_unfiltered.csv"), stringsAsFactors = F, header = T, check.names=FALSE)
      hist_fpts_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",gsub(" ", "", contest_info$Contest_Name[ind_match])), "/hist_fpts_mat.csv"), stringsAsFactors = F, header = T, check.names=F)
      
      # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
      write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat_unfiltered.csv"), row.names = F)
      write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat_unfiltered.csv"), row.names = F)
      write.csv(hist_fpts_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hist_fpts_mat.csv"), row.names = F)
    }
    
    print(paste0("Completed: ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), ": ", nrow(temp_julia_hitter_df), "=", nrow(cov_mat)))
  }
}




####### Section IV (covariance matrices - contest - apply filter) #######
print("Creating contest covariance matrices (no filtered)...")

dates_last <- seq(from = as.Date(date.start) - 2, to  = as.Date(date.end) - 2, by = "day") # date range
for (d in 1:length(dates_last)) {
  # load contest info file
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
  
  # subset by date
  date_last <- dates_last[d] # end date in covariance matrix function
  contest_info <- contest_info[contest_info$Contest_Date==(as.Date(date_last)+1),] # +1 to avoid look ahead bias
  print(paste0("End Date in createRollingCovarianceMatrix function: ", contest_info$Contest_Date[1]))
  
  # iterate through the contests (only call createRollingCovarianceMatrix when contest's corresponding Match_ID hasn't been run yet)
  for (i in 1:nrow(contest_info)) {
    print(paste0("Begin (Contest ", i, " / ", nrow(contest_info),"): ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i]))))
    
    # read in hist_fpts_mat and unfiltered covar matrix
    hist_fpts_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hist_fpts_mat.csv"), stringsAsFactors = F, header = T)
    cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat_unfiltered.csv"), stringsAsFactors = F, header = T, check.names=FALSE)
    
    # basic filter applied to all covariance matrices
    for (j in 1:nrow(hist_fpts_mat)) {
      # set covariance to 0 if either player in pair played less than 40% of games
      if (sum(is.na(hist_fpts_mat[j,])) > round(ncol(hist_fpts_mat)*(1-0.4))) {
        # set entire row and column for player in covar matrix to 0 (besides variance entry on diagonal)
        cov_mat[j,-j] <- 0
        cov_mat[-j,j] <- 0
      }
    }
    
    # apply "test" filter and write to file
    filter_name = "test"
    cov_mat <- filterCovarianceMatrix(contest_date = contest_info$Contest_Date[i], cov_mat_unfiltered = cov_mat, filter_name = filter_name)
    write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat_", filter_name, ".csv"), row.names = F)
    
    print(paste0("Completed: ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), ": ", nrow(temp_julia_hitter_df), "=", nrow(cov_mat)))
  }
}

