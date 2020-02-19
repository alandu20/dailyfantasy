if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Constructs the covariance matrix for MVO.
#
# TODO:
# - determine if pairwise.complete.obs is appropriate for our problem. considered dangerous
# - add opposing pitcher into covariance matrix calculation


####### Import Libraries #######
library(stringr)


####### Aggregate All Player Data for Each Day #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
dates <- seq(from = as.Date("2017-04-02"), to = as.Date("2017-04-04"), by = "day")
# dates <- seq(from = as.Date("2017-04-02"), to = Sys.Date() - 1, by = "day")
list_all_players <- NULL
for (d in 1:length(dates)) {
  # subset contest_info by date
  temp_contest_info <- contest_info[contest_info$Contest_Date==dates[d],]
  
  # get list of players for the day
  list_players_day <- NULL
  for (i in 1:nrow(temp_contest_info)) {
    temp_hitters <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
    temp_pitchers <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/pitchers.csv"), stringsAsFactors = F, header = T)
    
    temp_hitters <- temp_hitters[, c("Position", "Name", "Salary", "GameInfo", "AvgPointsPerGame", "teamAbbrev", "Actual_fpts")]
    temp_pitchers <- temp_pitchers[, c("Position", "Name", "Salary", "GameInfo", "AvgPointsPerGame", "teamAbbrev", "Actual_fpts")]
    
    temp_players_day <- rbind(temp_hitters, temp_pitchers)
    temp_players_day$Date <- dates[d]
    list_players_day <- rbind(list_players_day, temp_players_day)
  }
  
  # append to the unique rows (players) to the running list
  list_all_players <- rbind(list_all_players, unique(list_players_day))
  remove(list_players_day) # remove temp
}

####### Construct Matrix of Historical Fpts #######
# list of unique player names and their position
temp_names <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
names_unique_players <- unique(temp_names)

# initialize historical fpts matrix
hist_fpts_mat <- as.data.frame(matrix(data = NA, nrow = length(names_unique_players), ncol = length(dates)))
colnames(hist_fpts_mat) <- dates
rownames(hist_fpts_mat) <- names_unique_players

# fill historical fpts matrix (slow way)
for (i in 1:nrow(hist_fpts_mat)) {
  for (j in 1:ncol(hist_fpts_mat)) {
    temp <- list_all_players$Actual_fpts[paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)==rownames(hist_fpts_mat)[i] & list_all_players$Date==colnames(hist_fpts_mat)[j]]
    if (length(temp)==0) {
      hist_fpts_mat[i,j] <- NA
    } else {
      hist_fpts_mat[i,j] <- temp
    }
  }
}

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
  
  # find row index in list_all_players corresponding to the (x_i, y_i) element of the desired matrix (slightly slower)
  # for (i in 1:nrow(z)) {
  #   temp_inds_match_name <- which(temp_teamabbrev_all==z$name_teamAbbrev[i])
  #   temp_inds_match_date <- which(list_all_players$Date==z$date[i])
  #   temp_ind_match <- temp_inds_match_name[temp_inds_match_name %in% temp_inds_match_date] # matched row index in list_all_players
  #   if (length(temp_ind_match)!=0) {
  #     z$ind_match[i] <- temp_ind_match # matched row index in list_all_players
  #   }
  # }
  
  # find row index in list_all_players corresponding to the (x_i, y_i) element of the desired matrix (faster way)
  find_row_index <- function(x) {
    temp_inds_match_name <- which(temp_teamabbrev_all==x[3])
    temp_inds_match_date <- which(list_all_players$Date==x[4])
    temp_ind_match <- temp_inds_match_name[temp_inds_match_name %in% temp_inds_match_date] # matched row index in list_all_players
    if (length(temp_ind_match)!=0) {
      return(temp_ind_match) # matched row index in list_all_players
    }
    return(NA) # return NA if not in list_all_players (i.e. player didn't play that day)
  }
  z$ind_match <- apply(X = z, MARGIN = 1, FUN = find_row_index)
  
  # append Actual_fpts using ind_match
  z$Actual_fpts <- list_all_players$Actual_fpts[z$ind_match]
  
  return(z$Actual_fpts)
}

# fill historical fpts matrix (fast way)
hist_fpts_mat <- as.data.frame(outer(1:nrow(hist_fpts_mat), 1:ncol(hist_fpts_mat), FUN=fill_hist_mat))
# x <- sort(rep(1:nrow(hist_fpts_mat), ncol(hist_fpts_mat))) # debug
# y <- rep(1:ncol(hist_fpts_mat), nrow(hist_fpts_mat)) # debug
colnames(hist_fpts_mat) <- dates
rownames(hist_fpts_mat) <- names_unique_players

# remove rows with NA count > round(length(dates)/2)
inds.remove <- NULL
for (i in 1:nrow(hist_fpts_mat)) {
  if (sum(is.na(hist_fpts_mat[i,])) > round(length(dates)/2)) {
    inds.remove <- c(inds.remove, i)
  }
}
hist_fpts_mat <- hist_fpts_mat[-inds.remove,]


####### Construct Covariance Matrix #######
# construct full covariance matrix (misleading bc includes independent events)
cov_mat_temp <- cov(t(hist_fpts_mat), use = "pairwise.complete.obs") # complete.obs

# construct covariance matrix, only computed when players are on same team [TODO: or playing an opposing pitcher]
# initialize covariance matrix
cov_mat <- cov_mat_temp
cov_mat[,] <- NA

# initialize matrix of counts of non-NA game used for covariance calculation
cov_mat_counts <- cov_mat_temp
cov_mat_counts[,] <- NA

# transpose for covariance function
hist_fpts_mat_temp <- as.data.frame(t(hist_fpts_mat))

# fill elements above diagonal in covariance matrix (slow way)
# for (i in 1:nrow(cov_mat)) {
#   for (j in i:ncol(cov_mat)) {
#     temp <- hist_fpts_mat_temp[,i] # one player row of historical fpts
#     for (k in 1:nrow(hist_fpts_mat_temp)) {
#       # must be on same team
#       if (str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][i] != str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][j]) {
#         temp[k] <- NA # only one of the vectors needs to be set to NA to ensure exclusion from covariance calculation
#       }
#       
#       # TODO: or opposing pitcher
#     }
#     cov_mat[i,j] <- cov(temp, hist_fpts_mat_temp[,j], use = "pairwise.complete.obs") # covariance
#     cov_mat_counts[i,j] <- sum(which(!is.na(temp)) %in% which(!is.na(hist_fpts_mat_temp[,j]))) # number of games used in covariance calculatino
#     
#     print(paste0("Entries [", i, ", ", j, ") Completed"))
#   }
# }


# function to construct covariance matrix (faster way)
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
  only_keep_teammates <- function(x) {
    team_player_a <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][x[1]] # team of player a
    team_player_b <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][x[2]] # team of player b
    if (team_player_a != team_player_b) {
      return(NA) # exclude from covariance calculation
    } else {
      return(cov(x[3:(3+temp_num_dates-1)], x[(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)], use = "pairwise.complete.obs")) # compute covariance
    }
  }
  
  # subset by upper half of covariance matrix (save half the computation)
  cov_upper <- z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]),]
  cov_upper$covariance <- apply(X = cov_upper, MARGIN = 1, FUN = only_keep_teammates)
  
  # compute covariance for the subsetted part of matrix
  z$covariance <- NA
  z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]), 'covariance'] <- cov_upper$covariance
  
  # z$covariance <- apply(X = z, MARGIN = 1, FUN = only_keep_teammates) # (computes entire covariance matrix)
  
  return(z$covariance)
}

# contruct covariance matrix (fast way)
cov_mat <- as.data.frame(outer(1:nrow(cov_mat), 1:ncol(cov_mat), FUN=fill_cov_mat))
# x <- sort(rep(1:nrow(cov_mat), ncol(cov_mat))) # debug
# y <- rep(1:ncol(cov_mat), nrow(cov_mat)) # debug
colnames(cov_mat) <- rownames(hist_fpts_mat)
rownames(cov_mat) <- rownames(hist_fpts_mat)


# remove the diagonal (variances)
cov_mat_unique <- cov_mat
temp_inds <- 1:nrow(cov_mat_unique)
for (i in 1:length(temp_inds)) {
  cov_mat_unique[temp_inds[i], temp_inds[i]] <- NA 
}

# find the top n largest covariances (decreasing order)
cov_mat_unique <- as.matrix(cov_mat_unique)
n <- 25
x <- which(cov_mat_unique >= sort(cov_mat_unique, decreasing = T)[n], arr.ind = T)
x.order <- order(cov_mat_unique[x], decreasing = T) # decreasing order
x[x.order,]

for (i in 1:n) {
  player_a_ind <- x[x.order,][i,][1]
  player_b_ind <- x[x.order,][i,][2]
  player_a <- rownames(cov_mat_unique)[player_a_ind]
  player_b <- rownames(cov_mat_unique)[player_b_ind]
  print(paste0(player_a, ", ", player_b, ", Covariance: ", cov_mat_unique[player_a_ind, player_b_ind], ", Num_Games: ", cov_mat_counts[player_a_ind, player_b_ind]))
}








# debugging
# # function to construct covariance matrix (fast way)
# fill_cov_mat <- function(x, y) {
#   # vectorize indicies for outer function
#   # each (x_i, y_i) pair is the indicies of an element in the matrix we desire to construct
#   z <- as.data.frame(cbind(x, y))
#   
#   # append player_x and player_y historical fpts columns to z for fast vectorized computation
#   temp_num_dates <- nrow(hist_fpts_mat_temp)
#   z[,3:(3+temp_num_dates-1)] <- hist_fpts_mat[z$x,]
#   z[,(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)] <- hist_fpts_mat[z$y,]
#   
#   # inds of upper half of covariance matrix
#   temp_mat <- upper.tri(cov_mat, diag = TRUE)
#   inds_upper_cov <- which(temp_mat, arr.ind = T)
#   
#   # function for computing covariance, excluding when players not on same team
#   only_keep_teammates <- function(x) {
#     team_player_a <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(x[1])] # team of player a
#     team_player_b <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(x[2])] # team of player b
#     if (team_player_a != team_player_b) {
#       cov_and_count <- list(NA, 0) # exclude from covariance calculation, count of non-NAs is 0
#       return(cov_and_count)
#     } else {
#       # subset for readibility
#       player_a_hist_fpts <- x[3:(3+temp_num_dates-1)]
#       player_b_hist_fpts <- x[(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)]
#       
#       # compute covariance
#       cov_row <- cov(as.numeric(player_a_hist_fpts), as.numeric(player_b_hist_fpts), use = "pairwise.complete.obs")
#       
#       # count of non-NAs used in covariance calculation
#       count_row <- sum(which(!is.na(player_a_hist_fpts)) %in% which(!is.na(player_b_hist_fpts)))
#       
#       # combine
#       cov_and_count <- list(cov_row, count_row)
#       return(cov_and_count)
#     }
#   }
#   
#   # subset by indicies of upper half of covariance matrix (save half the computation)
#   cov_upper <- z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]),]
#   
#   # compute covariance for the subsetted indicies
#   asdf <- apply(X = cov_upper, MARGIN = 1, FUN = only_keep_teammates)
#   asdf1 <- sapply(asdf, `[[`, 1)
#   asdf2 <- sapply(asdf, `[[`, 2)
#   cov_upper$covariance <- asdf1
#   cov_upper$count_games <- asdf2
#   z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]), 'covariance'] <- paste0(cov_upper$covariance, cov_upper$count_games)
#   z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]), 'count_games'] <- cov_upper$count_games
#   
#   return(z$covariance)
#   # return(list(z$covariance, z$count_games))
# }