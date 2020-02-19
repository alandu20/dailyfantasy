if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for constructing a confirmed batting order matrix over a set of dates.
#
# Arguments:
# - vector of player names and their team, in the format "Name_Team"
# - vector of dates_vec
# - minimum fraction of games played

createHistoricalBOMatrix <- function(name_team_vec, list_all_players, dates_vec, min_games_pctg) {
  # initialize historical fpts matrix
  hist_bo_mat <- as.data.frame(matrix(data = NA, nrow = length(name_team_vec), ncol = length(dates_vec)))
  colnames(hist_bo_mat) <- dates_vec
  rownames(hist_bo_mat) <- name_team_vec
  
  # function that vectorizes the historical fpts matrix filling code (fast way)
  fill_hist_mat <- function(x, y) {
    # vectorize indicies for outer function
    # each (x_i, y_i) pair is the indicies of an element in the matrix we desire to construct
    z <- as.data.frame(cbind(x, y))
    
    # we want to get the Actual_fpts at each (x_i, y_i)
    # at each (x_i, y_i) we know the name_teamAbbrev and date
    # we need to match that with the name_teamAbbrev and date in list_all_players, then take the Actual_fpts
    z$name_teamAbbrev <- rownames(hist_bo_mat)[x]
    z$date <- colnames(hist_bo_mat)[y]
    
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
    z$Batting_Order_Confirmed <- list_all_players$Batting_Order_Confirmed[z$ind_match]
    
    return(z$Batting_Order_Confirmed)
  }
  # x <- sort(rep(1:nrow(hist_bo_mat), ncol(hist_bo_mat))) # debug
  # y <- rep(1:ncol(hist_bo_mat), nrow(hist_bo_mat)) # debug
  # fill historical fpts matrix (fast way)
  hist_bo_mat <- as.data.frame(outer(1:nrow(hist_bo_mat), 1:ncol(hist_bo_mat), FUN=fill_hist_mat))
  colnames(hist_bo_mat) <- dates_vec
  rownames(hist_bo_mat) <- name_team_vec
  
  # adjust for NAs if constructing full covaraince matrix to save time (contest matrices done separately)
  if (!is.null(min_games_pctg)) {
    # remove rows with NA count > round(length(dates_vec)*0.5)
    inds.remove <- NULL
    for (i in 1:nrow(hist_bo_mat)) {
      if (sum(is.na(hist_bo_mat[i,])) > round(length(dates_vec)*(1-min_games_pctg))) {
        inds.remove <- c(inds.remove, i)
      }
    }
    hist_bo_mat <- hist_bo_mat[-inds.remove,]
  }
  
  return(hist_bo_mat)
}