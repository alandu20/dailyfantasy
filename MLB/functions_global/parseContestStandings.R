if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Preprocesses contest-standings.csv files.


####### Begin Function #######
parseContestStandings <- function(contest.date, contest.name) {
  # load libraries
  require(stringr)
  
  # import functions
  source("MLB/functions_global/cleanPlayerNames.R")
  
  # read in contest standings
  temp.results <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/", contest.name, "/contest-standings.csv"), stringsAsFactors = F, header = T)
  
  # remove useless columns
  temp.results$TimeRemaining <- NULL
  temp.results$X <- NULL
  temp.results$Player <- NULL
  temp.results$X.Drafted <- NULL
  temp.results$FPTS <- NULL
  
  # initialize columns for each position
  temp.ind <- ncol(temp.results)
  temp.results[,(temp.ind+1):(temp.ind+10)] <- NA
  colnames(temp.results)[(temp.ind+1):(temp.ind+10)] <- c('P1','P2','C','1B','2B','3B','SS','OF1','OF2','OF3')
  
  # function for parsing the Lineup column
  splitPlayers <- function(x) {
    # remove Jr., Sr., middle initial (code won't work if name format isn't: first last)
    x <- cleanPlayerNames(x)
    
    # split at spaces
    temp <- str_split_fixed(x, " ", 30)
    
    # concatenate elements 1:3, 4:6, etc to create a vector with elements SS first last, 1B first last, etc.
    temp_vec <- NULL
    for (i in seq(1,30,3)) {
      temp_vec <- c(temp_vec, paste0(temp[i], " ", temp[i+1], " ", temp[i+2]))
    }
    
    # sort the vector in alphabetical order (uses the fact that every element begins with a position label: SS, 1B, etc.)
    temp_vec <- sort(temp_vec)
    
    # reorder into the desired order: P | P | C | 1B | 2B | 3B | SS | OF
    temp_vec <- c(temp_vec[8], temp_vec[9], temp_vec[4], temp_vec[1], temp_vec[2], temp_vec[3], temp_vec[10], temp_vec[5], temp_vec[6], temp_vec[7])
    
    # collapse vector to string
    temp_vec <- paste(temp_vec, collapse = " ")
    
    # split string, removing the position labels
    return(str_split_fixed(temp_vec, "P | P | C | 1B | 2B | 3B | SS | OF ", 11)[2:11])
  }
  
  
  
  # splitPlayers <- function(x) {
  #   return(str_split_fixed(x, "P | P | C | 1B | 2B | 3B | SS | OF ", 11)[2:11])
  # }
  
  
  
  # fill position columns by parsing Lineup column
  temp.players <- data.frame(matrix(unlist(lapply(X = temp.results$Lineup[1:nrow(temp.results)], FUN = splitPlayers)), nrow=nrow(temp.results), byrow=T))
  temp.results[,(temp.ind+1):(temp.ind+10)] <- temp.players
  
  # split EntryName column into username and entry number
  temp.entry <- str_split_fixed(temp.results$EntryName, " ", 2)
  temp.results$User_Name <- temp.entry[,1]
  temp.results$Entry_Num <- temp.entry[,2]
  
  return(temp.results)
}


# debugging
# contest.date <- "2017-04-23"
# contest.name <- "$4.00entry_MLB$35KFOUR-SEAMER(AFTERNOON)"





