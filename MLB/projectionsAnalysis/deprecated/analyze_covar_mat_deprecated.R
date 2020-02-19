if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Analyze covaraiance matrices over time. Most of this code is also used in the 
# filterCovarianceMatrix.R function.


####### Import Functions #######
source("MLB/functions_global/createRollingCovarianceMatrix.R")


####### Construct Covariance and Counts Matrix and Write to CSV file #######
# set date
contest.date <- as.Date("2017-04-26")

# read in 
cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/covariance_mat.csv"), stringsAsFactors = F, header = T, check.names = F)
cov_mat_counts <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/covariance_counts_mat.csv"), stringsAsFactors = F, header = T, check.names = F)

# copy upper triangle of cov matrix into lower triangle
cov_mat[lower.tri(cov_mat)] <- NA


####### Output Player Pairs with Highest Covariance #######
# remove the diagonal (variances)
temp_inds <- 1:nrow(cov_mat)
for (i in 1:length(temp_inds)) {
  cov_mat[temp_inds[i], temp_inds[i]] <- NA 
}

# find the top n largest covariances (decreasing order)
cov_mat <- as.matrix(cov_mat)
n <- ncol(cov_mat)
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

# remove players where Num_Games < (contest.date-2017-04-02)*0.5
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

# loop through dates
for (i in 1:length(dates)) {
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


####### Plot Covariance of the Top num_top_pairs Pairs (on Latest Date) over Time #######
# plotting top pairs
num_top_pairs <- 25

# subset cov_time by num_top_pairs
cov_time_temp <- cov_time[1:num_top_pairs, ]

plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(min(cov_time_temp, na.rm = T), max(cov_time_temp, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
# plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
for (i in 2:num_top_pairs) {
  points(as.numeric(cov_time_temp[i,]), type = 'b', col = i)
}
legend(x = "topleft", legend = c(rownames(cov_time_temp)), lwd = 1, col = 1:num_top_pairs, cex = 0.5)

# x axis: number of times covariance has increased by threshold
# y axis: current covariances
# filter out pairs that are big covariance for low x


####### Plot covariance (on end_day) vs sum(delta(cov) > threshold) ####### 
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
  if (sum(cov_time_chg[i,] > threshold, na.rm = T) > 1) {
    cov_time$temp[i] <- TRUE
  } else {
    cov_time$temp[i] <- FALSE
  }
}

# remove pairs not exceeding change threshold sufficiently much
cov_time_filtered <- cov_time[cov_time$temp==TRUE,]
cov_time$temp <- NULL
cov_time_filtered$temp <- NULL

# subset cov_time by num_top_pairs
cov_time_temp <- cov_time_filtered[1:num_top_pairs, ]

plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(min(cov_time_temp, na.rm = T), max(cov_time_temp, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
# plot(as.numeric(cov_time_temp[1,]), type = 'b', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
for (i in 2:num_top_pairs) {
  points(as.numeric(cov_time_temp[i,]), type = 'b', col = i)
}
legend(x = "topleft", legend = c(rownames(cov_time_temp)), lwd = 1, col = 1:num_top_pairs, cex = 0.5)



