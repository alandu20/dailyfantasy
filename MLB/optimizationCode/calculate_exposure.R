if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Calculates exposure to each player for a given set of lineups.

contest_date <- "2017-07-07"
entry_fee <- "$44.00"
contest_name <- "MLB$350KMediumAllStarBreakSpecial"
lineup_file <- "formulations.formulation7_covar_stacksize_5_overlap_5_lineups_150_lambda_0.0_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.3_exposure_SS0.5_exposure_OF0.6_min_pitcher_exposure0.6_covar_chg75p_exp(spike)"
lineup.data <- read.csv(file = paste0("MLB/data_warehouse/", contest_date,"/" , paste0(entry_fee,"entry_",gsub(" ", "", contest_name)), "/lineups/", lineup_file, ".csv"), stringsAsFactors = F, header = T)

# All players
occurences <- sort(table(unlist(lineup.data)), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# Pitchers
occurences <- sort(table(unlist(lineup.data[,c("P", "P.1")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# Catchers
occurences <- sort(table(unlist(lineup.data[,c("C")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# 1st baseman
occurences <- sort(table(unlist(lineup.data[,c("X1B")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# 2nd baseman
occurences <- sort(table(unlist(lineup.data[,c("X2B")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# 3rd baseman
occurences <- sort(table(unlist(lineup.data[,c("X3B")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# SS baseman
occurences <- sort(table(unlist(lineup.data[,c("SS")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# Outfielders
occurences <- sort(table(unlist(lineup.data[,c("OF", "OF.1", "OF.2")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))
