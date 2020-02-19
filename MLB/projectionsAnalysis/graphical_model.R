if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Import Libraries #######
require(stringr)
require(glmnet)
require(igraph)
require(plyr)


###### Import Functions #######
source("MLB/functions_global/createHistoricalFptsMatrix.R")
source("MLB/functions_global/aggregateAllPlayerResults.R")
source("MLB/functions_global/createHistoricalBOMatrix.R")


#####
baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)
date.start <- "2017-04-02"
k <- 23
contest_date <- baseline_contests$Date[k]
julia_hitter_df <- NULL # read.csv(file = paste0("MLB/data_warehouse/", contest_date, "/", baseline_contests$Contest_names[k], "/hitters.csv"), stringsAsFactors = F, header = T)
min_games_pctg <- NULL

date.end <- as.Date(contest_date) - 1 # offset by 1 to avoid lookahead bias
#####



# date sequence
dates <- seq(from = as.Date(date.start), to = as.Date(date.end), by = "day")

####### Aggregate All Player Data for Each Day #######
list_all_players <- aggregateAllPlayerResults(dates, julia_hitter_df)

####### Construct Matrix of Historical Fpts #######
# list of unique player names and their position
temp_names <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
if (is.null(julia_hitter_df)) {
  names_unique_players <- unique(temp_names)
} else {
  names_unique_players <- paste0(julia_hitter_df$Name, "_", julia_hitter_df$teamAbbrev) # must match order of julia_hitter_df (this line should be equivlaent to running unique(temp_names) but different order)
}

# call createHistoricalFptsMatrix function
hist_fpts_mat <- createHistoricalFptsMatrix(name_team_vec = names_unique_players, list_all_players = list_all_players, dates_vec = dates, min_games_pctg = min_games_pctg)

# call createHistoricalBOMatrix function
hist_bo_mat <- createHistoricalBOMatrix(name_team_vec = names_unique_players, list_all_players = list_all_players, dates_vec = dates, min_games_pctg = min_games_pctg)
hist_bo_mat[hist_bo_mat=="x"] <- NA # set "x" batting order (DNP) to NA

####### Create Graphical Model #######
# split by team
temp_team_inds <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2]
temp_all_teams <- unique(temp_team_inds)
if (length(which(temp_all_teams=="NA")) != 0) {
  temp_all_teams <- temp_all_teams[-which(temp_all_teams=="NA")]
}

for (team in temp_all_teams) {
  # make copy
  hist_fpts_mat_copy <- hist_fpts_mat
  
  # get most common BO for graph labels
  for (i in 1:nrow(hist_bo_mat)) {
    hist_fpts_mat_copy$BO_Mode[i] <- which.max(tabulate(as.numeric(hist_bo_mat[i,])))
  }
  
  temp_name <- team
  hist_fpts_mat_copy <- hist_fpts_mat_copy[which(temp_team_inds == temp_name),]
  
  # Delete rows with 50% of more games NA
  hist_fpts_mat_clean <- NULL
  for (i in 1:nrow(hist_fpts_mat_copy)) {
    if (sum(is.na(hist_fpts_mat_copy[i,1:(ncol(hist_fpts_mat_copy)-1)])) < (ncol(hist_fpts_mat_copy)-1)*0.50) { # 0.75
      hist_fpts_mat_clean <- rbind(hist_fpts_mat_clean, hist_fpts_mat_copy[i,])
    }
  }
  
  # store BO column separately
  bo_mode <- hist_fpts_mat_clean$BO_Mode
  hist_fpts_mat_clean$BO_Mode <- NULL
  
  # remove columns that are entirely NA
  temp_del_inds <- NULL
  for (i in 1:ncol(hist_fpts_mat_clean)) {
    if (sum(is.na(hist_fpts_mat_clean[,i]))==nrow(hist_fpts_mat_clean)) {
      temp_del_inds <- append(temp_del_inds, i)
    }
  }
  hist_fpts_mat_clean <- hist_fpts_mat_clean[,-c(temp_del_inds)]
  
  # Replace NA's with mean of row
  for (i in 1:dim(hist_fpts_mat_clean)[1]) {
    for (j in 1:dim(hist_fpts_mat_clean)[2]) {
      if (is.na(hist_fpts_mat_clean[i,j])) {
        hist_fpts_mat_clean[i,j] <- rowMeans(hist_fpts_mat_clean, na.rm = TRUE)[i]
      }
    }
  }
  
  # Initialize adjacency matrix
  # In an adjacency matrix, Mjk = 1, if there is an edge between the j-th and k-th nodes of the graph. Mjk = 0, otherwise. (The diagonal entries do not matter.)
  d <- dim(hist_fpts_mat_clean)[1]
  M <- matrix(0, d, d)
  
  # For each player on the team, apply Lasso so that we can find the neighborhood of every node and thus recover the whole graph
  mat <- matrix(0, d, d)
  for(i in 1:dim(hist_fpts_mat_clean)[1]){
    mat[i,] <- append(coef(glmnet(x=t(hist_fpts_mat_clean[-i,]), y=t(hist_fpts_mat_clean[i,]), family='gaussian', lambda=1))[-1], 0, after = (i-1))
  }
  
  # Player A's fpts is associated with Player B's fpts if Lasso chooses a non-zero coefficient for the variable associated with Player B
  for (i in 1:d) {
    for (j in 1:d) {
      if (mat[i,j]!=0 & mat[j,i]!=0) {
        M[i,j] <- 1
        M[j,i] <- 1
      }
    }
  }
  
  # Visualize
  ag=graph.adjacency(M, mode="undirected")
  V(ag)$colors=ifelse(degree(ag) < 3, 'SkyBlue2', 'red')
  par(mai=c(0,0,0,0))
  plot.igraph(ag, vertex.color=V(ag)$colors, vertex.size=6, vertex.label.cex=0.8, layout=layout_nicely(ag), vertex.label = bo_mode)
  title(team)
  
  # List the players that correspond to the red nodes in the graph
  print(paste0(team, ": ", length(which(V(ag)$colors=="red"))))
  print(rownames(hist_fpts_mat_clean)[which(V(ag)$colors=="red")])
}

print(contest_date)


# analysis
temp <- read.csv(file = paste0("MLB/data_warehouse/projections/dailyfantasynerd/updates/hitters_", contest_date, ".csv"))
temp <- temp[,c("Player.Name", "Pos", "Salary", "Team", "Opp", "Floor.FP", "Ceil.FP", "Proj.FP", "Actual.FP")]
temp <- temp[order(temp$Actual.FP, decreasing = T),]

temp <- temp[temp$Actual.FP > quantile(temp$Actual.FP, 0.90),]

temp_counts <- count(temp$Team)
temp_counts <- temp_counts[order(temp_counts$freq, decreasing = T),]
temp_counts


