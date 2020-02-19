if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function that applies a given filter to the covariance matrix.
# Implemented filters: test, chg75p_spike, chg75p_exp(spike), chg75p_zeros, arima_p3d1q2


filterCovarianceMatrix <- function(contest_date, cov_mat_unfiltered, filter_name, contest_entry_fee = NULL, contest_name = NULL) {
  ####### Import Libraries #######
  require(stringr)
  require(forecast)
  
  ####### Import Functions #######
  source("MLB/functions_global/listMissingDates.R")
  
  
  ####### Load Covariance and Counts Matrices #######
  # set date
  contest.date <- as.Date(contest_date)
  
  # read in full covariance matrix
  cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/covariance_mat.csv"), stringsAsFactors = F, header = T, check.names = F)
  cov_mat_counts <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/covariance_counts_mat.csv"), stringsAsFactors = F, header = T, check.names = F)
  
  # set lower triangle to NA to avoid duplicates
  cov_mat[lower.tri(cov_mat)] <- NA
  
  
  ####### Find Player Pairs with Highest Covariance #######
  # remove the diagonal (variances)
  temp_inds <- 1:nrow(cov_mat)
  for (i in 1:length(temp_inds)) {
    cov_mat[temp_inds[i], temp_inds[i]] <- NA 
  }
  
  # find the top n largest covariances (decreasing order)
  cov_mat <- as.matrix(cov_mat)
  n <- ncol(cov_mat) # maybe use sum(cov_mat > 0, na.rm = T) instead? current number is arbitrary
  x <- which(cov_mat >= sort(cov_mat, decreasing = T)[n], arr.ind = T)
  x.order <- order(cov_mat[x], decreasing = T) # decreasing order
  
  top_cov_pairs <- as.data.frame(matrix(data = NA, nrow = n, ncol = 4, dimnames = list(NULL, c("Player_A","Player_B","Covariance","Num_Games"))))
  for (i in 1:n) {
    player_a_ind <- x[x.order,][i,][1]
    player_b_ind <- x[x.order,][i,][2]
    
    top_cov_pairs$Player_A[i] <- colnames(cov_mat)[player_a_ind]
    top_cov_pairs$Player_B[i] <- colnames(cov_mat)[player_b_ind]
    top_cov_pairs$Covariance[i] <- cov_mat[player_a_ind, player_b_ind]
    top_cov_pairs$Num_Games[i] <- cov_mat_counts[player_a_ind, player_b_ind]
  }
  
  # we will entirely ignore player pairs that have played in less than 40% of games (safeguard)
  top_cov_pairs <- top_cov_pairs[top_cov_pairs$Num_Games > (as.Date(contest.date) - as.Date("2017-04-02"))*0.4, ]
  
  # view
  # View(top_cov_pairs)
  
  
  ####### Compute Covariance of the Top num_top_pairs Pairs (on Latest Date) over Time #######
  # Important note:
  # Several of the elements in a row (corresponding to the covariance between a pair of players)
  # are equal, which occurs when the two players aren't playing with each other.
  
  # set date range
  end_day <- contest.date
  dates <- seq(from = as.Date("2017-04-05"), to = end_day, by = "day")
  
  # initializations
  list_cov_mats <- list()
  cov_time <- as.data.frame(matrix(data = NA, nrow = nrow(top_cov_pairs), ncol = length(dates), dimnames = list(paste0(top_cov_pairs$Player_A[1:nrow(top_cov_pairs)], ", ", top_cov_pairs$Player_B[1:nrow(top_cov_pairs)]), as.character(dates))))
  missing_dates <- listMissingDates()
  
  # loop through dates
  for (i in 1:length(dates)) {
    # skip if no data for this date
    if (!(dates[i] %in% missing_dates)) {
      temp_cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", dates[i], "/covariance_mat.csv"), header = T, stringsAsFactors = F, check.names=FALSE)
      list_cov_mats[[i]] <- temp_cov_mat
      
      for (j in 1:nrow(top_cov_pairs)) {
        ind_a <- which(colnames(temp_cov_mat) == top_cov_pairs$Player_A[j])
        ind_b <- which(colnames(temp_cov_mat) == top_cov_pairs$Player_B[j])
        if (length(ind_a)!=0 & length(ind_b)!=0) {
          cov_time[j,i] <- temp_cov_mat[ind_a, ind_b] 
        }
      }
    }
  }
  
  
  ####### Find "Good" Player Pairs (where "good" is defined in different ways) #######
  ####### Filter: "test" #######
  if (filter_name == "test") {
    # initialize and fill change covariance matrix
    cov_time_chg <- cov_time
    cov_time_chg[,] <- NA
    for (i in 1:nrow(cov_time_chg)) {
      cov_time_chg[i,2:ncol(cov_time_chg)] <- cov_time[i,2:ncol(cov_time)] - cov_time[i,1:(ncol(cov_time)-1)]
    }
    
    # 75th percentile of change matrix (> 0)
    threshold <- quantile(unlist(cov_time_chg[cov_time_chg>0]), na.rm = T, 0.75)
    threshold
    for (i in 1:nrow(cov_time_chg)) {
      # at least 2 increases in covariance by an amount > 75th percentile of all changes
      if (sum(cov_time_chg[i,] > threshold, na.rm = T) > 1) {
        cov_time$temp[i] <- TRUE
      } else {
        cov_time$temp[i] <- FALSE
      }
    }
    
    # remove pairs not exceeding change threshold sufficiently much
    if (sum(cov_time$temp==TRUE)==0) {
      warning(paste0("(", contest_date, ") ", "No player pairs satisfied filtering criteria. Not difference between unfiltered matrix and filtered matrix."))
      return(cov_mat_unfiltered)
    }
    cov_time_filtered <- cov_time[cov_time$temp==TRUE,]
    cov_time$temp <- NULL
    cov_time_filtered$temp <- NULL
    print(paste0("Number of 'good' player pairs: ", nrow(cov_time_filtered)))
    
    # define how many player pairs to plot
    num_top_pairs <- min(25, nrow(cov_time_filtered))
    
    # subset cov_time by num_top_pairs
    cov_time_temp <- cov_time_filtered[1:num_top_pairs, ]
    
    plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(min(cov_time_temp, na.rm = T), max(cov_time_temp, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    # plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    for (i in 2:num_top_pairs) {
      points(as.numeric(cov_time_temp[i,]), type = 'b', col = i)
    }
    legend(x = "topleft", legend = c(rownames(cov_time_temp)), lwd = 1, col = 1:num_top_pairs, cex = 0.3)
    
    ####### Adjust values in input (unfiltered) covariance matrix #######
    # get names
    list_player_a <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,1]
    list_player_b <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,2]
    
    # copy
    cov_mat_filtered <- cov_mat_unfiltered
    
    # adjust values of the "good" (matched) players in the input/unfiltered matrix
    for(i in 1:nrow(cov_time_filtered)) {
      # find inds of players
      ind_a <- which(colnames(cov_mat_unfiltered)==list_player_a[i])
      ind_b <- which(colnames(cov_mat_unfiltered)==list_player_b[i])
      
      # scale up by 1.25
      cov_mat_filtered[ind_a, ind_b] <- cov_mat_filtered[ind_a, ind_b]*1.25
      cov_mat_filtered[ind_b, ind_a] <- cov_mat_filtered[ind_b, ind_a]*1.25 
    }
    
    return(cov_mat_filtered) 
  }
  
  
  ####### Filter: "chg75p_spike" #######
  if (filter_name == "chg75p_spike") {
    # initialize and fill change covariance matrix
    cov_time_chg <- cov_time
    cov_time_chg[,] <- NA
    for (i in 1:nrow(cov_time_chg)) {
      cov_time_chg[i,2:ncol(cov_time_chg)] <- cov_time[i,2:ncol(cov_time)] - cov_time[i,1:(ncol(cov_time)-1)]
    }
    
    # 75th percentile of change matrix (> 0)
    threshold <- quantile(unlist(cov_time_chg[cov_time_chg>0]), na.rm = T, 0.75)
    threshold
    for (i in 1:nrow(cov_time_chg)) {
      # in at least 20% of games where pair played, increase in covariance by an amount > 75th percentile of all changes
      if (sum(cov_time_chg[i,] > threshold, na.rm = T) >= max(2, round(sum(cov_time_chg[i,] != 0, na.rm = T)*0.20))) {
        cov_time$temp[i] <- TRUE
      } else {
        cov_time$temp[i] <- FALSE
      }
    }
    
    # remove pairs not exceeding change threshold sufficiently much
    if (sum(cov_time$temp==TRUE)==0) {
      warning(paste0("(", contest_date, ") ", "No player pairs satisfied filtering criteria. Not difference between unfiltered matrix and filtered matrix."))
      return(cov_mat_unfiltered)
    }
    cov_time_filtered <- cov_time[cov_time$temp==TRUE,]
    cov_time$temp <- NULL
    cov_time_filtered$temp <- NULL
    print(paste0("Number of 'good' player pairs: ", nrow(cov_time_filtered)))
    
    # define how many player pairs to plot
    num_top_pairs <- min(25, nrow(cov_time_filtered))
    
    # subset cov_time by num_top_pairs
    cov_time_temp <- cov_time_filtered[1:num_top_pairs, ]
    
    plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(min(cov_time_temp, na.rm = T), max(cov_time_temp, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    # plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    for (i in 2:num_top_pairs) {
      points(as.numeric(cov_time_temp[i,]), type = 'b', col = i)
    }
    legend(x = "topleft", legend = c(rownames(cov_time_temp)), lwd = 1, col = 1:num_top_pairs, cex = 0.3)
    
    ####### Adjust values in input (unfiltered) covariance matrix #######
    # get names
    list_player_a <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,1]
    list_player_b <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,2]
    
    # copy
    cov_mat_filtered <- cov_mat_unfiltered
    
    # adjust values of the "good" (matched) players in the input/unfiltered matrix
    for(i in 1:nrow(cov_time_filtered)) {
      # find inds of players
      ind_a <- which(colnames(cov_mat_unfiltered)==list_player_a[i])
      ind_b <- which(colnames(cov_mat_unfiltered)==list_player_b[i])
      
      # scale up by 1.25
      cov_mat_filtered[ind_a, ind_b] <- cov_mat_filtered[ind_a, ind_b]*1.25
      cov_mat_filtered[ind_b, ind_a] <- cov_mat_filtered[ind_b, ind_a]*1.25 
    }
    
    return(cov_mat_filtered) 
  }
  
  
  ####### Filter: "chg75p_exp(spike)" #######
  if (filter_name == "chg75p_exp(spike)") {
    # initialize and fill change covariance matrix
    cov_time_chg <- cov_time
    cov_time_chg[,] <- NA
    for (i in 1:nrow(cov_time_chg)) {
      cov_time_chg[i,2:ncol(cov_time_chg)] <- cov_time[i,2:ncol(cov_time)] - cov_time[i,1:(ncol(cov_time)-1)]
    }
    
    # 75th percentile of change matrix (> 0)
    threshold <- quantile(unlist(cov_time_chg[cov_time_chg>0]), na.rm = T, 0.75)
    threshold
    for (i in 1:nrow(cov_time_chg)) {
      # in at least 20% of games where pair played, increase in covariance by an amount > 75th percentile of all changes
      if (sum(cov_time_chg[i,] > threshold, na.rm = T) >= max(2, round(sum(cov_time_chg[i,] != 0, na.rm = T)*0.20))) {
        cov_time$temp[i] <- TRUE
      } else {
        cov_time$temp[i] <- FALSE
      }
    }
    
    # remove pairs not exceeding change threshold sufficiently much
    if (sum(cov_time$temp==TRUE)==0) {
      warning(paste0("(", contest_date, ") ", "No player pairs satisfied filtering criteria. Not difference between unfiltered matrix and filtered matrix."))
      return(cov_mat_unfiltered)
    }
    cov_time_filtered <- cov_time[cov_time$temp==TRUE,]
    cov_time$temp <- NULL
    cov_time_filtered$temp <- NULL
    print(paste0("Number of 'good' player pairs: ", nrow(cov_time_filtered)))
    
    # define how many player pairs to plot
    num_top_pairs <- min(25, nrow(cov_time_filtered))
    
    # subset cov_time by num_top_pairs
    cov_time_temp <- cov_time_filtered[1:num_top_pairs, ]
    
    plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(min(cov_time_temp, na.rm = T), max(cov_time_temp, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    # plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    for (i in 2:num_top_pairs) {
      points(as.numeric(cov_time_temp[i,]), type = 'b', col = i)
    }
    legend(x = "topleft", legend = c(rownames(cov_time_temp)), lwd = 1, col = 1:num_top_pairs, cex = 0.3)
    
    ####### Adjust values in input (unfiltered) covariance matrix #######
    # get names
    list_player_a <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,1]
    list_player_b <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,2]
    
    # copy
    cov_mat_filtered <- cov_mat_unfiltered
    
    # define function that scales the elements of a vector between 0 and 1. append scaled column to cov_time_filtered
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    cov_time_filtered$scaled_covar <- range01(cov_time_filtered[,ncol(cov_time_filtered)])
    cov_time_filtered$scaled_covar <- 1 + exp((-1)*cov_time_filtered$scaled_covar)*0.25
    
    # adjust values of the "good" (matched) players in the input/unfiltered matrix
    for(i in 1:nrow(cov_time_filtered)) {
      # find inds of players
      ind_a <- which(colnames(cov_mat_unfiltered)==list_player_a[i])
      ind_b <- which(colnames(cov_mat_unfiltered)==list_player_b[i])
      
      # scale up by 1+exp(cov_time_filtered$scaled_covar[i])
      cov_mat_filtered[ind_a, ind_b] <- cov_mat_filtered[ind_a, ind_b]*cov_time_filtered$scaled_covar[i]
      cov_mat_filtered[ind_b, ind_a] <- cov_mat_filtered[ind_b, ind_a]*cov_time_filtered$scaled_covar[i]
    }
    
    return(cov_mat_filtered) 
  }
  
  # this will only really work when there is a reasonable number of "good" pairs
  if (filter_name == "chg75p_zeros") {
    # initialize and fill change covariance matrix
    cov_time_chg <- cov_time
    cov_time_chg[,] <- NA
    for (i in 1:nrow(cov_time_chg)) {
      cov_time_chg[i,2:ncol(cov_time_chg)] <- cov_time[i,2:ncol(cov_time)] - cov_time[i,1:(ncol(cov_time)-1)]
    }
    
    # 75th percentile of change matrix (> 0)
    threshold <- quantile(unlist(cov_time_chg[cov_time_chg>0]), na.rm = T, 0.75)
    threshold
    for (i in 1:nrow(cov_time_chg)) {
      # in at least 20% of games where pair played, increase in covariance by an amount > 75th percentile of all changes
      if (sum(cov_time_chg[i,] > threshold, na.rm = T) >= max(2, round(sum(cov_time_chg[i,] != 0, na.rm = T)*0.20))) {
        cov_time$temp[i] <- TRUE
      } else {
        cov_time$temp[i] <- FALSE
      }
    }
    
    # remove pairs not exceeding change threshold sufficiently much
    if (sum(cov_time$temp==TRUE)==0) {
      warning(paste0("(", contest_date, ") ", "No player pairs satisfied filtering criteria. Not difference between unfiltered matrix and filtered matrix."))
      return(cov_mat_unfiltered)
    }
    cov_time_filtered <- cov_time[cov_time$temp==TRUE,]
    cov_time$temp <- NULL
    cov_time_filtered$temp <- NULL
    print(paste0("Number of 'good' player pairs: ", nrow(cov_time_filtered)))
    
    # define how many player pairs to plot
    num_top_pairs <- min(25, nrow(cov_time_filtered))
    
    # subset cov_time by num_top_pairs
    cov_time_temp <- cov_time_filtered[1:num_top_pairs, ]
    
    plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(min(cov_time_temp, na.rm = T), max(cov_time_temp, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    # plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    for (i in 2:num_top_pairs) {
      points(as.numeric(cov_time_temp[i,]), type = 'b', col = i)
    }
    legend(x = "topleft", legend = c(rownames(cov_time_temp)), lwd = 1, col = 1:num_top_pairs, cex = 0.3)
    
    ####### Adjust values in input (unfiltered) covariance matrix #######
    # get names
    list_player_a <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,1]
    list_player_b <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,2]
    
    # init 0 matrix
    cov_mat_filtered <- cov_mat_unfiltered # copy for dimensions and rc names
    cov_mat_filtered[upper.tri(cov_mat_filtered, diag = F)] <- 0 # keep diag, otherwise init to 0
    cov_mat_filtered[lower.tri(cov_mat_filtered, diag = F)] <- 0
    
    # fill 0 matrix with values of the "good" (matched) players in the input/unfiltered matrix
    for(i in 1:nrow(cov_time_filtered)) {
      # find inds of players
      ind_a <- which(colnames(cov_mat_unfiltered)==list_player_a[i])
      ind_b <- which(colnames(cov_mat_unfiltered)==list_player_b[i])
      
      # insert values of unfiltered covariance matrix (only good pairs' covariances)
      cov_mat_filtered[ind_a, ind_b] <- cov_mat_unfiltered[ind_a, ind_b]
      cov_mat_filtered[ind_b, ind_a] <- cov_mat_unfiltered[ind_b, ind_a]
    }
    
    return(cov_mat_filtered)
  }
  
  
  ####### Filter: "chg75p_zeros" #######
  # this will only really work when there is a reasonable number of "good" pairs
  if (filter_name == "chg75p_zeros") {
    # initialize and fill change covariance matrix
    cov_time_chg <- cov_time
    cov_time_chg[,] <- NA
    for (i in 1:nrow(cov_time_chg)) {
      cov_time_chg[i,2:ncol(cov_time_chg)] <- cov_time[i,2:ncol(cov_time)] - cov_time[i,1:(ncol(cov_time)-1)]
    }
    
    # 75th percentile of change matrix (> 0)
    threshold <- quantile(unlist(cov_time_chg[cov_time_chg>0]), na.rm = T, 0.75)
    threshold
    for (i in 1:nrow(cov_time_chg)) {
      # in at least 20% of games where pair played, increase in covariance by an amount > 75th percentile of all changes
      if (sum(cov_time_chg[i,] > threshold, na.rm = T) >= max(2, round(sum(cov_time_chg[i,] != 0, na.rm = T)*0.20))) {
        cov_time$temp[i] <- TRUE
      } else {
        cov_time$temp[i] <- FALSE
      }
    }
    
    # remove pairs not exceeding change threshold sufficiently much
    if (sum(cov_time$temp==TRUE)==0) {
      warning(paste0("(", contest_date, ") ", "No player pairs satisfied filtering criteria. Not difference between unfiltered matrix and filtered matrix."))
      return(cov_mat_unfiltered)
    }
    cov_time_filtered <- cov_time[cov_time$temp==TRUE,]
    cov_time$temp <- NULL
    cov_time_filtered$temp <- NULL
    print(paste0("Number of 'good' player pairs: ", nrow(cov_time_filtered)))
    
    # define how many player pairs to plot
    num_top_pairs <- min(25, nrow(cov_time_filtered))
    
    # subset cov_time by num_top_pairs
    cov_time_temp <- cov_time_filtered[1:num_top_pairs, ]
    
    plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(min(cov_time_temp, na.rm = T), max(cov_time_temp, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    # plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
    for (i in 2:num_top_pairs) {
      points(as.numeric(cov_time_temp[i,]), type = 'b', col = i)
    }
    legend(x = "topleft", legend = c(rownames(cov_time_temp)), lwd = 1, col = 1:num_top_pairs, cex = 0.3)
    
    ####### Adjust values in input (unfiltered) covariance matrix #######
    # get names
    list_player_a <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,1]
    list_player_b <- str_split_fixed(rownames(cov_time_filtered), ", ", 2)[,2]
    
    # init 0 matrix
    cov_mat_filtered <- cov_mat_unfiltered # copy for dimensions and rc names
    cov_mat_filtered[upper.tri(cov_mat_filtered, diag = F)] <- 0 # keep diag, otherwise init to 0
    cov_mat_filtered[lower.tri(cov_mat_filtered, diag = F)] <- 0
    
    # fill 0 matrix with values of the "good" (matched) players in the input/unfiltered matrix
    for(i in 1:nrow(cov_time_filtered)) {
      # find inds of players
      ind_a <- which(colnames(cov_mat_unfiltered)==list_player_a[i])
      ind_b <- which(colnames(cov_mat_unfiltered)==list_player_b[i])
      
      # insert values of unfiltered covariance matrix (only good pairs' covariances)
      cov_mat_filtered[ind_a, ind_b] <- cov_mat_unfiltered[ind_a, ind_b]
      cov_mat_filtered[ind_b, ind_a] <- cov_mat_unfiltered[ind_b, ind_a]
    }
    
    return(cov_mat_filtered)
  }
  
  
  ####### Filter: "arima_p3d1q2" #######
  if (filter_name == "arima_p3d1q2") {
    ####### Import Libraries #######
    require(forecast)
    
    
    ####### Compute Covariance of Player Pairs (on Latest Date) over Time FOR UNFILTERED MATRIX #######
    # read in UNFILTERED covariance and counts matrix
    # cov_mat_unfiltered_temp <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/", paste0(contest_entry_fee,"entry_",gsub(" ", "", contest_name)), "/covariance_mat_unfiltered.csv"), stringsAsFactors = F, header = T, check.names = F)
    cov_mat_unfiltered_temp <- cov_mat_unfiltered # copy function argument
    cov_mat_counts_unfiltered <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/", paste0(contest_entry_fee,"entry_",gsub(" ", "", contest_name)), "/covariance_counts_mat_unfiltered.csv"), stringsAsFactors = F, header = T, check.names = F)
    
    # remove the diagonal (variances)
    temp_inds <- 1:nrow(cov_mat_unfiltered_temp)
    for (i in 1:length(temp_inds)) {
      cov_mat_unfiltered_temp[temp_inds[i], temp_inds[i]] <- NA 
    }
    
    # set lower triangle to NA to avoid duplicates
    cov_mat_unfiltered_temp[lower.tri(cov_mat_unfiltered_temp)] <- NA
    
    # number of non-zero or non-NA elements in cov_mat_unfiltered_temp
    # quantile(cov_mat_unfiltered_temp[cov_mat_unfiltered_temp > 0 & !is.na(cov_mat_unfiltered_temp)])
    # temp_num_nonzero <- sum(cov_mat_unfiltered_temp > 0, na.rm = T)
    temp_num_nonzero <- sum(cov_mat_unfiltered_temp != 0, na.rm = T)
    cov_mat_unfiltered_temp[cov_mat_unfiltered_temp == 0] <- NA
    
    # find the top temp_num_nonzero (i.e. all positive) largest covariances (decreasing order)
    cov_mat_unfiltered_temp <- as.matrix(cov_mat_unfiltered_temp)
    x <- which(cov_mat_unfiltered_temp >= sort(cov_mat_unfiltered_temp, decreasing = T)[temp_num_nonzero], arr.ind = T)
    x.order <- order(cov_mat_unfiltered_temp[x], decreasing = T) # decreasing order
    
    cov_pairs_unfiltered <- as.data.frame(matrix(data = NA, nrow = temp_num_nonzero, ncol = 4, dimnames = list(NULL, c("Player_A","Player_B","Covariance","Num_Games"))))
    for (i in 1:temp_num_nonzero) {
      player_a_ind <- x[x.order,][i,][1]
      player_b_ind <- x[x.order,][i,][2]
      
      cov_pairs_unfiltered$Player_A[i] <- colnames(cov_mat_unfiltered_temp)[player_a_ind]
      cov_pairs_unfiltered$Player_B[i] <- colnames(cov_mat_unfiltered_temp)[player_b_ind]
      cov_pairs_unfiltered$Covariance[i] <- cov_mat_unfiltered_temp[player_a_ind, player_b_ind]
      cov_pairs_unfiltered$Num_Games[i] <- cov_mat_counts_unfiltered[player_a_ind, player_b_ind]
    }
    
    # remove pairs in cov_pairs_unfiltered that played in 33% of days (to save computation time)
    cov_pairs_unfiltered <- cov_pairs_unfiltered[cov_pairs_unfiltered$Num_Games > (as.Date(contest.date) - as.Date("2017-04-02"))*0.33, ]
    
    # Covariance over time
    # set date range
    end_day <- contest.date
    dates <- seq(from = as.Date("2017-04-05"), to = end_day, by = "day")
    
    # initializations
    cov_time_unfiltered <- as.data.frame(matrix(data = NA, nrow = nrow(cov_pairs_unfiltered), ncol = length(dates), dimnames = list(paste0(cov_pairs_unfiltered$Player_A[1:nrow(cov_pairs_unfiltered)], ", ", cov_pairs_unfiltered$Player_B[1:nrow(cov_pairs_unfiltered)]), as.character(dates))))
    
    # loop through dates
    for (i in 1:length(dates)) {
      temp_cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", dates[i], "/covariance_mat.csv"), header = T, stringsAsFactors = F, check.names=FALSE)
      
      for (j in 1:nrow(cov_pairs_unfiltered)) {
        ind_a <- which(colnames(temp_cov_mat) == cov_pairs_unfiltered$Player_A[j])
        ind_b <- which(colnames(temp_cov_mat) == cov_pairs_unfiltered$Player_B[j])
        if (length(ind_a)!=0 & length(ind_b)!=0) {
          cov_time_unfiltered[j,i] <- temp_cov_mat[ind_a, ind_b] 
        }
      }
      # print(dates[i])
    }
    
    # plot covariances
    plot(as.numeric(cov_time_unfiltered[1,]), type = 'l', col = 1, ylim = c(min(cov_time_unfiltered, na.rm = T), max(cov_time_unfiltered, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Player Pairs by Covariance (as of ", end_day-1, ")"))
    for (i in 2:nrow(cov_pairs_unfiltered)) {
      points(as.numeric(cov_time_unfiltered[i,]), type = 'l', col = i)
    }
    
    # add ARIMA forecast column to cov_time_unfiltered
    # require(forecast)
    cov_time_unfiltered$pred <- NA
    for (i in 1:nrow(cov_time_unfiltered)) {
      # remove games that pair didn't play, i.e. no change in covariance from prior game (set to NA)
      temp_row <- cov_time_unfiltered[i, 1:(ncol(cov_time_unfiltered)-1)]
      temp_chg <- c(NA, cov_time_unfiltered[i,2:(ncol(cov_time_unfiltered)-1)] - cov_time_unfiltered[i,1:(ncol(cov_time_unfiltered)-2)])
      temp_row[temp_chg==0 | is.na(temp_chg)] <- NA
      cov_time_unfiltered[i, 1:(ncol(cov_time_unfiltered)-1)] <- temp_row
      
      # predict next covariance (subset out NAs in ARIMA input)
      # predicting on last game played (could be an issue if last game was 5/2 and prior to that 4/15, that is, pair didn't play for extended period of time. TODO.)
      temp_row <- temp_row[!is.na(temp_row)]
      t <- try(forecast(arima(as.numeric(temp_row), order=c(3,1,2), method="ML"), h=1))
      if ("try-error" %in% class(t)) {
        cov_time_unfiltered$pred[i] <- mean(temp_row[length(temp_row):(length(temp_row)-2)])
        print(paste0("Pair ", i, ": used average of last 3 games instead b/c algorithm did not converge"))
      } else {
        temp_pred <- forecast(arima(as.numeric(temp_row), order=c(3,1,2), method="ML"), h=1)
        cov_time_unfiltered$pred[i] <- temp_pred$mean
      }
      
      # plot every 10
      if (i %% 10 == 0) {
        plot(as.numeric(cov_time_unfiltered[i,]), type = "b") # all points
        points(ncol(cov_time_unfiltered), as.numeric(cov_time_unfiltered$pred[i]), col = "red") # prediction in red
      }
    }
    
    
    ####### Adjust values in input (unfiltered) covariance matrix #######
    # get names
    list_player_a <- str_split_fixed(rownames(cov_time_unfiltered), ", ", 2)[,1]
    list_player_b <- str_split_fixed(rownames(cov_time_unfiltered), ", ", 2)[,2]
    
    # init 0 matrix
    cov_mat_filtered <- cov_mat_unfiltered # copy for dimensions and rc names (not using cov_mat_unfiltered_temp b/c set lower triangle to NA)
    cov_mat_filtered[upper.tri(cov_mat_filtered, diag = F)] <- 0 # keep diag, otherwise init to 0
    cov_mat_filtered[lower.tri(cov_mat_filtered, diag = F)] <- 0
    
    # fill 0 matrix with values of the "good" (matched) players in the input/unfiltered matrix
    for(i in 1:nrow(cov_time_unfiltered)) {
      # find inds of players
      ind_a <- which(colnames(cov_mat_filtered)==list_player_a[i])
      ind_b <- which(colnames(cov_mat_filtered)==list_player_b[i])
      
      # insert values of unfiltered covariance matrix (only good pairs' covariances)
      cov_mat_filtered[ind_a, ind_b] <- cov_time_unfiltered$pred[i]
      cov_mat_filtered[ind_b, ind_a] <- cov_time_unfiltered$pred[i]
    }
    
    return(cov_mat_filtered)
  }
  
  stop("Filter name not found.")
  return(NULL)
}


# debugging
# cov_mat_unfiltered <- read.csv(file = "MLB/data_warehouse/2017-04-28/$55.00entry_DKFiveYearAnniversary$55/covariance_mat_unfiltered.csv", stringsAsFactors = F, header = T, check.names = F)
# contest_date <- "2017-04-28"
# filter_name <- "ols"
# contest_entry_fee = "$55.00"
# contest_name = "DKFiveYearAnniversary$55"

# contest_date <- "2017-04-10"
# cov_mat_unfiltered <- cov_mat
