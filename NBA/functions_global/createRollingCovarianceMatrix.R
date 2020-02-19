if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for constructing the covariance matrix for MVO. Only computed for players on same team.
#
# Arguments:
# - julia_df: set to NULL if computing full covariance matrix, otherwise pass in hitter df for a contest
# - min_games_pctg: set between 0-1
#
# TODO:
# - consider other correlation/covariance measures (spearman, kendall)
# - determine if pairwise.complete.obs is appropriate for our problem. considered dangerous
#
# Notes:
# - we remove players that have played less than half the games
# - consider using Vectorize library in the future to vectorize for outer function rather than code up yourself


####### Function for Computing Covariance Matrix Given Start and End Date #######
createRollingCovarianceMatrix <- function(date.start, date.end, julia_df, min_games_pctg) {
  ####### Import Libraries #######
  require(stringr)
  
  
  ###### Import Functions #######
  source("NBA/functions_global/createHistoricalFptsMatrix.R")
  source("NBA/functions_global/aggregateAllPlayerResults.R")
  source("NBA/functions_global/listMissingDates.R")
  
  
  # date sequence
  dates <- seq(from = as.Date(date.start), to = as.Date(date.end), by = "day")
  if (length(which(dates %in% listMissingDates())) != 0) {
    dates <- dates[-which(dates %in% listMissingDates())] # remove missing dates
  }
  
  
  ####### Aggregate All Player Data for Each Day #######
  list_all_players <- aggregateAllPlayerResults(dates, julia_df)
  
  
  ####### Construct Matrix of Historical Fpts #######
  # print("Constructing Historical Fpts Matrix...")
  
  # list of unique player names and their position
  temp_names <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
  if (is.null(julia_df)) {
    names_unique_players <- unique(temp_names)
  } else {
    names_unique_players <- paste0(julia_df$Name, "_", julia_df$Team) # must match order of julia_df (this line should be equivlaent to running unique(temp_names) but different order)
    if (length(names_unique_players) != length(unique(names_unique_players))) {
      print("========== PROBLEM: SOMETHING WEIRD IN DKSALARIES.CSV ==========")
      print("For some reason there are duplicate players in DKSalaries.csv and hitters.csv. Examine files.")
      stop("See above. Fix and rerun.")
      names_unique_players <- unique(names_unique_players)
    }
  }
  
  # call createHistoricalFptsMatrix function
  hist_fpts_mat <- createHistoricalFptsMatrix(name_team_vec = names_unique_players, list_all_players = list_all_players, dates_vec = dates, min_games_pctg = min_games_pctg)

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
      if (row[1] %% row[2] == 0 & row[2] %% 50 == 0) {
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
        cov_row <- cov(as.numeric(player_a_hist_fpts), as.numeric(player_b_hist_fpts), use = "pairwise.complete.obs", method = "pearson")

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
    
  # return covariance matrix 
  return(list(cov_mat, hist_fpts_mat))
}

