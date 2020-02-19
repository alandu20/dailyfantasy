if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for constructing the covariance matrix for MVO. Only computed for players on same team.
#
# Arguments:
# - julia_hitter_df: set to NULL if computing full covariance matrix, otherwise pass in hitter df for a contest
# - filter: set to NULL if no filter, otherwise "test", "chg75p_spike", "chg75p_exp(spike)" (as defined in covariance_matrix_dictionary.txt)
#
# TODO:
# - determine if pairwise.complete.obs is appropriate for our problem. considered dangerous
# - add opposing pitcher into covariance matrix calculation
# - add opposing batters? (ballpark effect?)
#
# Notes:
# - we remove players that have played less than half the games
# - consider using Vectorize library in the future to vectorize for outer function rather than code up yourself


####### Function for Computing Covariance Matrix Given Start and End Date #######
createRollingCovarianceMatrix_deprecated <- function(date.start, date.end, julia_hitter_df, filter_name) {
  ####### Import Libraries #######
  library(stringr)
  
  
  ###### Import Functions #######
  source("MLB/functions_global/filterCovarianceMatrix.R")
  
  
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
      
      #
      temp_hitters <- temp_hitters[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts")]
      temp_pitchers <- temp_pitchers[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts")]
      if (is.null(julia_hitter_df)) {
        temp_players_day <- rbind(temp_hitters, temp_pitchers) 
      } else {
        temp_players_day <- temp_hitters
        # match hitters from the julia input file
        # temp_players_day <- temp_players_day[paste0(temp_players_day$Name, temp_players_day$teamAbbrev) %in% paste0(julia_hitter_df$Name, julia_hitter_df$teamAbbrev), ] # this generally should be an unnecessary check
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
  
  # initialize historical fpts matrix
  hist_fpts_mat <- as.data.frame(matrix(data = NA, nrow = length(names_unique_players), ncol = length(dates)))
  colnames(hist_fpts_mat) <- dates
  rownames(hist_fpts_mat) <- names_unique_players
  
  # function that vectorizes the historical fpts matrix filling code (fast way)
  fill_hist_mat <- function(x, y) {
    # vectorize indicies for outer function
    # each (x_i, y_i) pair is the indicies of an element in the matrix we desire to construct
    z <- as.data.frame(cbind(x, y))
    
    # we want to get the Actual_fpts at each (x_i, y_i)
    # at each (x_i, y_i) we know the name_teamAbbrev and date
    # we need to match that with the name_teamAbbrev and date in list_all_players, then take the Actual_fpts
    z$name_teamAbbrev <- rownames(hist_fpts_mat)[x]
    z$date <- colnames(hist_fpts_mat)[y]
    
    # temp vars and initializations
    temp_teamabbrev_all <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
    z$ind_match <- NA
    
    # find row index in list_all_players corresponding to the (x_i, y_i) element of the desired matrix (faster way)
    find_row_index <- function(row) {
      # if (as.numeric(row[1]) %% as.numeric(row[2]) == 0) {
      #   print(paste0(row[1], ", ", row[2]))
      # }

      temp_inds_match_name <- which(temp_teamabbrev_all==as.character(row[3])) # which(temp_teamabbrev_all==row[3]) # note: not hard coded
      temp_inds_match_date <- which(list_all_players$Date==as.Date(as.character(row[4]))) # which(list_all_players$Date==row[4]) # note: not hard coded
      temp_ind_match <- temp_inds_match_name[temp_inds_match_name %in% temp_inds_match_date] # matched row index in list_all_players
      if (length(temp_ind_match)!=0) {
        return(temp_ind_match) # matched row index in list_all_players
      }
      return(NA) # return NA if not in list_all_players (i.e. player didn't play that day)
    }
    z$ind_match <- apply(X = z, MARGIN = 1, FUN = find_row_index)
    
    # for (i in 1:nrow(z)) {
    #   temp_inds_match_name <- which(temp_teamabbrev_all==z$name_teamAbbrev[i])
    #   temp_inds_match_date <- which(list_all_players$Date==z$date[i])
    #   temp_ind_match <- temp_inds_match_name[temp_inds_match_name %in% temp_inds_match_date] # matched row index in list_all_players
    #   if (length(temp_ind_match)!=0) {
    #     z$ind_match[i] <- temp_ind_match # matched row index in list_all_players
    #   }
    # }
    
    # append Actual_fpts using ind_match
    z$Actual_fpts <- list_all_players$Actual_fpts[z$ind_match]
    
    return(z$Actual_fpts)
  }
  # x <- sort(rep(1:nrow(hist_fpts_mat), ncol(hist_fpts_mat))) # debug
  # y <- rep(1:ncol(hist_fpts_mat), nrow(hist_fpts_mat)) # debug
  # fill historical fpts matrix (fast way)
  hist_fpts_mat <- as.data.frame(outer(1:nrow(hist_fpts_mat), 1:ncol(hist_fpts_mat), FUN=fill_hist_mat))
  colnames(hist_fpts_mat) <- dates
  rownames(hist_fpts_mat) <- names_unique_players
  
  # adjust for NAs
  if (is.null(julia_hitter_df)) {
    # remove rows with NA count > round(length(dates)*0.5)
    inds.remove <- NULL
    for (i in 1:nrow(hist_fpts_mat)) {
      if (sum(is.na(hist_fpts_mat[i,])) > round(length(dates)*0.8)) {
        inds.remove <- c(inds.remove, i)
      }
    }
    hist_fpts_mat <- hist_fpts_mat[-inds.remove,]
  } else {
    # set rows with NA count > round(length(dates)*0.5) all to NA
    for (i in 1:nrow(hist_fpts_mat)) {
      if (sum(is.na(hist_fpts_mat[i,])) > round(length(dates)*0.6)) {
        hist_fpts_mat[i,] <- NA
      }
    }
  }
  
  
  ####### Construct Covariance Matrix #######
  # print("Constructing covariance matrix...")
  
  # construct full covariance matrix (misleading bc includes independent events)
  cov_mat_temp <- cov(t(hist_fpts_mat), use = "pairwise.complete.obs") # complete.obs
  
  # construct covariance matrix, only computed when players are on same team [TODO: or playing an opposing pitcher]
  # initialize covariance matrix
  cov_mat <- cov_mat_temp
  cov_mat[,] <- NA
  
  # transpose for covariance function
  hist_fpts_mat_temp <- as.data.frame(t(hist_fpts_mat))
  
  # function to construct covariance matrix (fast way)
  fill_cov_mat <- function(x, y) {
    # vectorize indicies for outer function
    # each (x_i, y_i) pair is the indicies of an element in the matrix we desire to construct
    z <- as.data.frame(cbind(x, y))
    
    # append player_x and player_y historical fpts columns to z for fast vectorized computation
    temp_num_dates <- nrow(hist_fpts_mat_temp)
    z[,3:(3+temp_num_dates-1)] <- hist_fpts_mat[z$x,]
    z[,(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)] <- hist_fpts_mat[z$y,]
    
    # inds of upper half of covariance matrix
    temp_mat <- upper.tri(cov_mat, diag = TRUE)
    inds_upper_cov <- which(temp_mat, arr.ind = T)
    
    # function for computing covariance, excluding when players not on same team
    only_keep_teammates <- function(row) {
      if (row[1] %% row[2] == 0 & row[2] %% 10 == 0) {
        # print(paste0("Index ", row[1], ", ", row[2], " / (", nrow(cov_mat), ", ", ncol(cov_mat), ")", " Completed"))
        print(paste0("Column ", row[2], " / ", nrow(cov_mat), " Completed"))
      }
      team_player_a <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[1])] # team of player a
      team_player_b <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[2])] # team of player b
      if (team_player_a != team_player_b) {
        return(NA) # exclude from covariance calculation
      } else {
        # subset for readibility
        player_a_hist_fpts <- row[3:(3+temp_num_dates-1)]
        player_b_hist_fpts <- row[(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)]
        
        # compute covariance
        cov_row <- cov(as.numeric(player_a_hist_fpts), as.numeric(player_b_hist_fpts), use = "pairwise.complete.obs")
        
        return(cov_row)
      }
    }
    
    # subset by upper half of covariance matrix (save half the computation)
    cov_upper <- z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]),]
    cov_upper$covariance <- apply(X = cov_upper, MARGIN = 1, FUN = only_keep_teammates)
    
    # compute covariance for the subsetted part of matrix
    z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]), 'covariance'] <- cov_upper$covariance
    
    return(z$covariance)
  }
  
  # contruct covariance matrix (fast way)
  cov_mat <- as.data.frame(outer(1:nrow(cov_mat), 1:ncol(cov_mat), FUN=fill_cov_mat))
  colnames(cov_mat) <- rownames(hist_fpts_mat)
  rownames(cov_mat) <- rownames(hist_fpts_mat)
  
  # copy upper triangle of cov matrix into lower triangle
  cov_mat[lower.tri(cov_mat)] <- t(cov_mat)[lower.tri(cov_mat)]
  
  # apply filtering
  if (!is.null(filter)) {
    cov_mat <- filterCovarianceMatrix(contest_date = as.Date(date.end)+1, cov_mat_unfiltered = cov_mat, filter_name = filter_name)
  }
  
  ####### Construct Covariance Counts Matrix #######
  # function to construct counts matrix (fast way)
  fill_cov_counts_mat <- function(x, y) {
    # vectorize indicies for outer function
    # each (x_i, y_i) pair is the indicies of an element in the matrix we desire to construct
    z <- as.data.frame(cbind(x, y))
    
    # append player_x and player_y historical fpts columns to z for fast vectorized computation
    temp_num_dates <- nrow(hist_fpts_mat_temp)
    z[,3:(3+temp_num_dates-1)] <- hist_fpts_mat[z$x,]
    z[,(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)] <- hist_fpts_mat[z$y,]
    
    # inds for count computation (don't need to compute for covariance NAs)
    temp_mat <- cov_mat
    temp_mat[is.na(temp_mat)] <- "FALSE" # needs to be string for some type reason
    temp_mat_inds <- which(temp_mat!="FALSE", arr.ind = T)
    
    # function for computing covariance, excluding when players not on same team
    only_keep_teammates_count_games <- function(row) {
      team_player_a <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[1])] # team of player a
      team_player_b <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[2])] # team of player b
      if (team_player_a != team_player_b) {
        cov_and_count <- list(NA, 0) # exclude from covariance calculation, count of non-NAs is 0
        return(0)
      } else {
        # subset for readibility
        player_a_hist_fpts <- row[3:(3+temp_num_dates-1)]
        player_b_hist_fpts <- row[(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)]
        
        # count of non-NAs used in covariance calculation
        count_row <- sum(which(!is.na(player_a_hist_fpts)) %in% which(!is.na(player_b_hist_fpts)))
        
        return(count_row)
      }
    }
    
    # subset by the non-NA covariance indicies (save computation)
    cov_temp_count <- z[paste0(z$x, ", ", z$y) %in% paste0(temp_mat_inds[,1], ", ", temp_mat_inds[,2]),]
    cov_temp_count$count_games <- apply(X = cov_temp_count, MARGIN = 1, FUN = only_keep_teammates_count_games)
    
    # compute covariance for the subsetted part of matrix
    z[paste0(z$x, ", ", z$y) %in% paste0(temp_mat_inds[,1], ", ", temp_mat_inds[,2]), 'count_games'] <- cov_temp_count$count_games
    
    return(z$count_games)
  }
  
  # contruct counts matrix (fast way)
  cov_mat_counts <- as.data.frame(outer(1:nrow(cov_mat), 1:ncol(cov_mat), FUN=fill_cov_counts_mat))
  colnames(cov_mat_counts) <- rownames(hist_fpts_mat)
  rownames(cov_mat_counts) <- rownames(hist_fpts_mat)
  
  # copy upper triangle of cov matrix into lower triangle
  cov_mat_counts[lower.tri(cov_mat_counts)] <- t(cov_mat_counts)[lower.tri(cov_mat_counts)]
  
  
  # return covariance matrix and counts matrix
  return(list(cov_mat, cov_mat_counts))
}


# debug
# date.start = "2017-04-02"
# date.end = "2017-04-04"
#
# julia_input_df = NULL
# contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
# i = 10
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
# date.end = contest_info$Contest_Date[i]

# date.start = "2017-04-02"
# date.end <- "2017-04-21"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", date.end,"/" , "$2.00entry_MLB$2DoubleUp", "/hitters.csv"), stringsAsFactors = F, header = T)


# date.start = "2017-04-02"
# date.end <- Sys.Date()-2
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)

# date.start = "2017-04-02"
# date.end = "2017-04-25"
# julia_hitter_df = NULL

# date.start = "2017-04-02"
# date.end <- "2017-04-09"
# julia_hitter_df <- temp_julia_hitter_df

# date.start <- "2017-04-02"
# date.end <- "2017-04-29"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/2017-04-29/$3.00entry_MLB$5KMoonshot(Afternoon)/hitters.csv"), stringsAsFactors = F, header = T)
# filter_on <- F


# asdf <- read.csv(file = "MLB/data_warehouse/2017-04-29/$33.00entry_MLB$110KFastball(Early)/covariance_mat_chg75p_exp(spike).csv", stringsAsFactors = F, header = T, check.names = F)


