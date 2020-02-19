if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

####### Description #######
# Function for identifying contests that have the same julia input file so that we don't
# need to run the covariance code multiple times for the same set of players.


findDuplicateContests <- function(contest_info) {
  # initializations
  contest_info$Match_ID <- NA
  list_contest <- NULL
  temp_ind <- 1
  
  # iterate
  for (i in 1:nrow(contest_info)) {
    # check if contest folder exists
    temp.dksalaries.path <- paste0(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/DKSalaries.csv"))
    if (file.exists(temp.dksalaries.path)) {
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
  }
  print(paste0("Number of contests with unique hitter.csv files: ", length(unique(contest_info$Match_ID))))
  
  return(contest_info)
}