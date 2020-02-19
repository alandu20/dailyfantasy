if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### DESCRIPTION #######
# Function that takes a lineup as input and returns the fpts, mean historical fpts,
# and sd historical fpts of each player in the lineup as output.

lineupFpts <- function(contest_date, contest_name, formulation_name, lineup_index) {
  # import libraries
  require(stringr)
  
  # import functions
  source("MLB/functions_global/cleanPlayerNames.R") 
  
  # load lineup file for input contest
  lineup_file <- read.csv(file = paste0("MLB/data_warehouse/", contest_date, "/", contest_name, "/lineups/", formulation_name, ".csv"), stringsAsFactors = F, header = T)
  
  # subset single lineup in lineup file (lineup_index)
  lineup <- lineup_file[lineup_index,]
  
  # initialize output df
  output.df <- as.data.frame(matrix(data = NA, nrow = length(lineup), ncol = 7, dimnames = list(NULL, c("Player", "Team", "Pos", "Fpts", "Avg_Hist_Fpts", "Sd_Hist_Fpts", "Num_Games"))))
  output.df$Player <- t(lineup)
  
  # load FC results to fill Fpts column in output.df
  temp_fc_results <- read.csv(paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/player_results.csv"), stringsAsFactors = F, header = T)
  temp_fc_results$Player <- cleanPlayerNames(temp_fc_results$Player)
  temp_bb_stats <- read.csv(paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/Daily_Player_Stats.csv"), stringsAsFactors = F, header = T)
  temp_bb_stats$Name <- cleanPlayerNames(temp_bb_stats$Name)
  
  # match player in output.df to FC results
  for (k in 1:length(lineup)) {
    temp_match <- temp_fc_results$Player==output.df$Player[k]
    if (length(which(temp_match)) == 1) {
      output.df$Team[k] <- temp_fc_results$Team[temp_match]
      output.df$Pos[k] <- temp_fc_results$Pos[temp_match]
      output.df$Fpts[k] <- temp_fc_results$Actual.Score[temp_match]
    } else if (length(which(temp_match)) > 1) {
      print(paste0("Warning: ", output.df$Player[k], " fpts score may be incorrect b/c same name on different teams."))
      output.df$Team[k] <- temp_fc_results$Team[temp_match][1] # use first index match
      output.df$Pos[k] <- temp_fc_results$Pos[temp_match][1]
      output.df$Fpts[k] <- temp_fc_results$Actual.Score[temp_match][1]
    } else {
      output.df$Team[k] <- NA
      output.df$Pos[k] <- NA
      output.df$Fpts[k] <- NA
      output.df$num_AB[k] <- NA
      output.df$batting_order[k] <- NA
    }
    
    # match bb_stats
    temp_match_stats <- temp_bb_stats$Name==output.df$Player[k]
    if (length(which(temp_match_stats)) == 1) {
      output.df$num_AB[k] <- temp_bb_stats$AB[temp_match_stats]
      output.df$batting_order[k] <- temp_bb_stats$Order[temp_match_stats]
    } else if (length(which(temp_match_stats)) > 1) {
      output.df$num_AB[k] <- temp_bb_stats$AB[temp_match_stats][1]
      output.df$batting_order[k] <- temp_bb_stats$Order[temp_match_stats][1]
    } else {
      output.df$num_AB[k] <- NA
      output.df$batting_order[k] <- NA
    }
  }
  
  # load hist_fpts_mat (batters) to fill Avg_Hist_Fpts column in output.df
  hist_fpts_mat <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/hist_fpts_mat.csv"), stringsAsFactors = F, header = T, check.names = F)
  
  # remove team name from player name
  hist_fpts_mat[,1] <- str_split_fixed(hist_fpts_mat[,1], "_", 2)[,1]
  
  # match player in output.df to hist_fpts_mat. compute mean and std dev
  for (k in 1:length(lineup)) {
    temp_match <- hist_fpts_mat[,1]==output.df$Player[k]
    if (length(which(temp_match)) == 1) {
      temp_fpts <- hist_fpts_mat[temp_match,][-1] # remove first column element (player sname)
    } else if (length(which(temp_match)) > 1) {
      temp_fpts <- hist_fpts_mat[temp_match,][1,-1] # use first index match
    } else {
      temp_fpts <- NA
    }
    output.df$Avg_Hist_Fpts[k] <- mean(as.numeric(temp_fpts), na.rm = T)
    output.df$Sd_Hist_Fpts[k] <- sd(as.numeric(temp_fpts), na.rm = T)
    output.df$Num_Games[k] <- sum(!is.na(temp_fpts))
  }
  
  return(output.df)
}

# load baseline contest info
baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)
baseline_contests <- baseline_contests[1:which(baseline_contests$Date=="2017-07-14"),]

# initialization
fpts_mat <- as.data.frame(matrix(data = NA, nrow = 150, ncol = nrow(baseline_contests), dimnames = list(paste0("Index ", 1:150), baseline_contests$Date)))
pnls_mat <- as.data.frame(matrix(data = NA, nrow = 150, ncol = nrow(baseline_contests), dimnames = list(paste0("Index ", 1:150), baseline_contests$Date)))

# iterate through all contests
for (i in 1:nrow(baseline_contests)) {
  # initalization
  temp_df <- as.data.frame(matrix(data = NA, nrow = 150, ncol = 2, dimnames = list(NULL, c("Total_Fpts","output.df"))))
  
  # load contest standings and payout structure
  contest_standings <- read.csv(paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/", baseline_contests$Contest_names[i], "/contest-standings.csv"), stringsAsFactors = F, header = T)
  payout_structure <- read.csv(paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/", baseline_contests$Contest_names[i], "/payout_structure.csv"), stringsAsFactors = F, header = T)
  
  # iterate through all lineups
  for (j in 1:150) {
    output <- lineupFpts(contest_date = baseline_contests$Date[i], contest_name = baseline_contests$Contest_names[i], formulation_name = "formulations.formulation5_covar_stacksize_5_overlap_5_lineups_150_lambda_0.001_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.3_exposure_SS0.5_exposure_OF0.6_covar_chg75p_exp(spike)", lineup_index = j)
    # "formulations.formulation7_covar_stacksize_5_overlap_5_lineups_150_lambda_0.0_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.5_exposure_SS0.5_exposure_OF0.6_min_pitcher_exposure0.5_covar_chg75p_exp(spike)"
    temp_df$Total_Fpts[j] <- sum(output$Fpts)
    temp_df$output.df[j] <- list(output)
    
    # compute pnl for the lineup
    if (!is.na(temp_df$Total_Fpts[j])) {
      temp_place <- which.min(abs(contest_standings$Points-temp_df$Total_Fpts[j])) # not precise but good estimate (can be done better probably)
      if (temp_place > payout_structure$Place_hi[nrow(payout_structure)]) {
        temp_payout <- 0
      } else {
        for (k in 1:nrow(payout_structure)) {
          if (temp_place >= payout_structure$Place_lo[k] && temp_place <= payout_structure$Place_hi[k]) {
            temp_payout <- payout_structure$Payout[k]
            break
          }
        }
      }
    } else {
      temp_payout <- NA
    }

    pnls_mat[j,i] <- temp_payout
  }
  
  # add to fpts_mat
  fpts_mat[,i] <- temp_df$Total_Fpts
  
  print(paste0("Completed ", baseline_contests$Date[i]))
}

# mean
plot(rowMeans(fpts_mat, na.rm = T), type = "b", ylab = "Mean Fpts Score", xlab = "Lineup Index", main = "Mean Fpts by Lineup Index (baseline_contests)")

# max
index_max <- rep(NA, 150)
for (i in 1:150) {
  index_max[i] <- max(fpts_mat[i,], na.rm = T)
}
plot(index_max, type = "b", ylab = "Max Fpts Score", xlab = "Lineup Index", main = "Max Fpts by Lineup Index (baseline_contests)")

# min
index_min <- rep(NA, 150)
for (i in 1:150) {
  index_min[i] <- min(fpts_mat[i,], na.rm = T)
}
plot(index_min, type = "b", ylab = "Min Fpts Score", xlab = "Lineup Index", main = "Min Fpts by Lineup Index (baseline_contests)")

# all
plot(rep(1, ncol(fpts_mat)), fpts_mat[1,], ylab = "Fpts Score", xlab = "Lineup Index", main = "Fpts by Lineup Index (baseline_contests)", xlim = c(1,150), ylim = c(min(fpts_mat, na.rm = T), max(fpts_mat, na.rm = T)))
for (i in 2:150) {
  points(rep(i, ncol(fpts_mat)), fpts_mat[i,])
}

# quantiles for each lineup index
boxplot(t(fpts_mat), ylab = "Fpts Score", xlab = "Lineup Index", main = "Box & Whisker: Fpts by Lineup Index (baseline_contests)")

# pct cashing
index_place <- rep(NA, 150)
for (i in 1:150) {
  index_place[i] <- sum(pnls_mat[i,] > 0, na.rm = T) / sum(!is.na(pnls_mat[i,]))
}
plot(index_place, type = "b", ylab = "% Cashing", xlab = "Lineup Index", main = "% Cashing by Lineup Index (baseline_contests)")


