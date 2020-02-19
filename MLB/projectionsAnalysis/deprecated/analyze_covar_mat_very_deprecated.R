####### Create Covariance Matrix for a Given Contest #######
# # load contest info file
# contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
# 
# # subset by date
# date <- "2017-04-05"
# contest_info <- contest_info[contest_info$Contest_Date==as.Date(date),]
# 
# for (i in 1:nrow(contest_info)) {
#   # read in julia input file for this date
#   temp_julia_input_hitter <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
#   
#   # change team name from DK to DFN naming convention for matching purposes
#   # temp_julia_input_hitter$teamAbbrev <- convertTeamNames(team_vec = temp_julia_input_hitter$teamAbbrev, from_source = "DK", to_source = "DFN")
#   
#   # create temporary name column for matching purposes
#   temp_julia_input_hitter$Temp_Name <- paste0(temp_julia_input_hitter$Name, "_", temp_julia_input_hitter$teamAbbrev)
#   
#   # initialize covariance matrix corresponding to temp_julia_input_hitter (this contest's hitters)
#   temp_cov_mat_julia <- as.data.frame(matrix(data = NA, nrow = length(temp_julia_input_hitter$Temp_Name), ncol = length(temp_julia_input_hitter$Temp_Name)))
#   colnames(temp_cov_mat_julia) <- temp_julia_input_hitter$Temp_Name
#   rownames(temp_cov_mat_julia) <- temp_julia_input_hitter$Temp_Name
#   
#   # read in full covariance matrix (hitters from all contests for the day)
#   temp_cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", date, "/covariance_mat.csv"), header = T, stringsAsFactors = F, check.names=FALSE)
#   rownames(temp_cov_mat) <- colnames(temp_cov_mat)
#   
#   # remove rows and columns from temp_cov_mat (all contests for day) that aren't in temp_cov_mat_julia (this contest)
#   inds_rc_remove <- which(!(colnames(temp_cov_mat) %in% colnames(temp_cov_mat_julia)))
#   temp_cov_mat <- temp_cov_mat[,-inds_rc_remove]
#   temp_cov_mat <- temp_cov_mat[-inds_rc_remove,]
#   
#   # debug
#   colnames(temp_cov_mat_julia)[which(!(colnames(temp_cov_mat_julia) %in% colnames(temp_cov_mat)))]
#   
#   # match r/c indicies of the players in temp_cov_mat that are in temp_cov_mat_julia (can't use which() b/c need to keep order)
#   inds_match <- NULL
#   for (i in 1:ncol(temp_cov_mat_julia)) {
#     inds_match <- c(inds_match, which(colnames(temp_cov_mat)==colnames(temp_cov_mat_julia)[i]))
#   }
#   inds_match
#   
#   # reorder temp_cov_mat based on inds_match (the order of players in the temp_cov_mat_julia)
#   temp_cov_mat <- temp_cov_mat[,inds_match] # reorder columns
#   temp_cov_mat <- temp_cov_mat[inds_match,] # reorder rows
#   
#   # check that everything is in order
#   inds_match_check <- NULL
#   for (i in 1:ncol(temp_cov_mat_julia)) {
#     inds_match_check <- c(inds_match_check, which(colnames(temp_cov_mat)==colnames(temp_cov_mat_julia)[i]))
#   }
#   inds_match_check # should be in ascending order
# }


# iterate through dates if desired
# dates <- seq(from = as.Date("2017-04-05"), to = Sys.Date() - 1, by = "day")
# for (d in 1:length(dates)) {
#   # load contest info file
#   contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
# 
#   # subset by date
#   date <- dates[d]
#   contest_info <- contest_info[contest_info$Contest_Date==as.Date(date),]
# 
#   for (i in 1:nrow(contest_info)) {
#     # read in julia input file for this date
#     temp_julia_input_hitter <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
# 
#     # change team name from DK to DFN naming convention for matching purposes
#     temp_julia_input_hitter$teamAbbrev <- convertTeamNames(team_vec = temp_julia_input_hitter$teamAbbrev, from_source = "DK", to_source = "DFN")
# 
#     # create temporary name column for matching purposes
#     temp_julia_input_hitter$Temp_Name <- paste0(temp_julia_input_hitter$Name, "_", temp_julia_input_hitter$teamAbbrev)
# 
#     # initialize covariance matrix corresponding to temp_julia_input_hitter
#     temp_cov_mat_julia <- as.data.frame(matrix(data = NA, nrow = length(temp_julia_input_hitter$Temp_Name), ncol = length(temp_julia_input_hitter$Temp_Name)))
#     colnames(temp_cov_mat_julia) <- temp_julia_input_hitter$Temp_Name
#     rownames(temp_cov_mat_julia) <- temp_julia_input_hitter$Temp_Name
# 
#     # read in covariance matrix
#     temp_cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i], "/covariance_mat.csv"), header = T, stringsAsFactors = F, check.names=FALSE)
#     rownames(temp_cov_mat) <- colnames(temp_cov_mat)
# 
#     # remove rows and columns from temp_cov_mat that aren't in temp_cov_mat_julia
#     inds_rc_remove <- which(!(colnames(temp_cov_mat) %in% colnames(temp_cov_mat_julia)))
#     temp_cov_mat <- temp_cov_mat[,-inds_rc_remove]
#     temp_cov_mat <- temp_cov_mat[-inds_rc_remove,]
# 
#     # match r/c indicies of the players in temp_cov_mat that are in temp_cov_mat_julia (can't use which() b/c need to keep order)
#     inds_match <- NULL
#     for (j in 1:ncol(temp_cov_mat_julia)) {
#       inds_match <- c(inds_match, which(colnames(temp_cov_mat)==colnames(temp_cov_mat_julia)[j]))
#     }
#     inds_match
# 
#     # reorder temp_cov_mat based on inds_match (the order of players in the temp_cov_mat_julia)
#     temp_cov_mat <- temp_cov_mat[,inds_match] # reorder columns
#     temp_cov_mat <- temp_cov_mat[inds_match,] # reorder rows
# 
#     # check that everything is in order
#     inds_match_check <- NULL
#     for (j in 1:ncol(temp_cov_mat_julia)) {
#       inds_match_check <- c(inds_match_check, which(colnames(temp_cov_mat)==colnames(temp_cov_mat_julia)[j]))
#     }
#     inds_match_check # should be in ascending order
# 
#     # write to file
#     write.csv(temp_cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat.csv"), row.names = F)
#   }
# }