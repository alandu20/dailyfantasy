if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for constructing a historical fantasy points matrix.
#
# Arguments:
# - vector of player names and their team, in the format "Name_Team"
# - vector of dates_vec
# - minimum fraction of games played

createHistoricalFptsMatrix <- function(name_team_vec, list_all_players, dates_vec, min_games_pctg) {
  # initialize historical fpts matrix
  hist_fpts_mat <- as.data.frame(matrix(data = NA, nrow = length(name_team_vec), ncol = length(dates_vec)))
  colnames(hist_fpts_mat) <- dates_vec
  rownames(hist_fpts_mat) <- name_team_vec
  
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
    temp_teamabbrev_all <- paste0(list_all_players$Name, "_", list_all_players$Team)
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
  colnames(hist_fpts_mat) <- dates_vec
  rownames(hist_fpts_mat) <- name_team_vec
  
  # adjust for NAs if constructing full covaraince matrix to save time (contest matrices done separately)
  if (!is.null(min_games_pctg)) {
    # remove rows with NA count > round(length(dates_vec)*0.5)
    inds.remove <- NULL
    for (i in 1:nrow(hist_fpts_mat)) {
      if (sum(is.na(hist_fpts_mat[i,])) > round(length(dates_vec)*(1-min_games_pctg))) {
        inds.remove <- c(inds.remove, i)
      }
    }
    hist_fpts_mat <- hist_fpts_mat[-inds.remove,]
  }
  
  return(hist_fpts_mat)
}