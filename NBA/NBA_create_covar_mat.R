# TODO:
# - the way historical matrix created is dumb, slows this down
# - note that begin date of covariance calc is hard coded

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create covariance matrix

####### Covariance matrices - contest - no filtering) #######
print("Creating contest covariance matrices (no filtering)...")

NBA_create_covar_mat <- function(date_start, date_end) {
  ####### Import Functions #######
  source("NBA/functions_global/createRollingCovarianceMatrix.R")
  source("NBA/functions_global/findDuplicateContests.R")
  
  dates_last <- seq(from = as.Date(date_start) - 2, to = as.Date(date_end) - 2, by = "day") # date range
  for (d in 1:length(dates_last)) {
    # load contest info file
    contest_info <- read.csv(file = 'NBA/data_warehouse/contests.csv', stringsAsFactors = F)
    
    # subset by date
    date_last <- dates_last[d] # end date in covariance matrix function
    contest_info <- contest_info[contest_info$Contest_Date==(as.Date(date_last)+1),] # +1 to avoid look ahead bias
    print(paste0("End Date in createRollingCovarianceMatrix function: ", contest_info$Contest_Date[1]))
    
    # identify contests that have the same julia input file so that we don't need to run the covariance code multiple times for the same set of players
    # print(paste0("Number of contests with unique hitter.csv files: ", length(unique(contest_info$Match_ID))))
    contest_info <- findDuplicateContests(contest_info)
    
    # iterate through the contests (only call createRollingCovarianceMatrix when contest's corresponding Match_ID hasn't been run yet)
    for (i in 1:nrow(contest_info)) {
      # check if contest folder exists
      temp.dksalaries.path <- paste0(file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/", paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]), "/DKSalaries.csv"))
      if (file.exists(temp.dksalaries.path)) {
        # read in julia input file for this date
        temp_julia_df <- read.csv(file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]), "/players.csv"), stringsAsFactors = F, header = T)
        
        print(paste0("Begin (Contest ", i, " / ", nrow(contest_info),"): ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i])))
        
        # if contest's hitters corresponding covar mat hasn't been computed already, then do so. else, use an existing covar mat
        if (contest_info$Match_ID[i] %in% contest_info$Match_ID[1:(i-1)] == FALSE | i==1) {
          print("Constructing covariance matrix for this contest...")
          
          # construct covariance and counts matrices
          cov.dat <- createRollingCovarianceMatrix(date.start = "2017-11-27", date.end = date_last, julia_df = temp_julia_df, min_games_pctg = NULL)
          cov_mat <- cov.dat[[1]]
          hist_fpts_mat <- cov.dat[[2]]
          
          # set NAs to 0 in covariance matrix for julia code
          cov_mat[is.na(cov_mat)] <- 0
          
          # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
          write.csv(cov_mat, file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]), "/covariance_mat_unfiltered.csv"), row.names = F)
          write.csv(hist_fpts_mat, file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]), "/hist_fpts_mat.csv"), row.names = T)
          
        } else {
          ind_match <- min(which(contest_info$Match_ID[1:i] %in% contest_info$Match_ID[i]))
          
          cov_mat <- read.csv(file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",contest_info$Contest_Name[ind_match]), "/covariance_mat_unfiltered.csv"), stringsAsFactors = F, header = T, check.names=FALSE)
          hist_fpts_mat <- read.csv(file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[ind_match],"/" , paste0(contest_info$Entry_Fee[ind_match],"entry_",contest_info$Contest_Name[ind_match]), "/hist_fpts_mat.csv"), stringsAsFactors = F, header = T, check.names=F)
          
          # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
          write.csv(cov_mat, file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]), "/covariance_mat_unfiltered.csv"), row.names = F)
          write.csv(hist_fpts_mat, file = paste0("NBA/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",contest_info$Contest_Name[i]), "/hist_fpts_mat.csv"), row.names = F)
        }
        
        print(paste0("Completed: ", contest_info$Contest_Date[i], " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), ": ", nrow(temp_julia_df), "=", nrow(cov_mat)))
      }
    }
  }
}

NBA_create_covar_mat(date_start = "2017-12-29", date_end = "2018-01-01")

