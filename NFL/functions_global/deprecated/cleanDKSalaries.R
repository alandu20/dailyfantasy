

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

####### Description #######
# Function to clean the DK Salaries file.

cleanDKSalaries <- function(data) {
  data["FirstName"] <- ""
  data["LastName"] <- ""
  data["Opponent"] <- ""
  
  colnames(data)[7] <- "Team"
  
  for (i in 1:nrow(data)) {
    gameInfo <- data[i, "GameInfo"]
    name <- data[i, "Name"]
    
    AwayTeam <- substr(gameInfo, 1, regexpr('@', gameInfo) - 1)
    
    HomeTeam <- substr(gameInfo, regexpr('@', gameInfo) + 1, regexpr(' ', gameInfo) - 1)
    
    FirstName <- substr(name, 1, regexpr(' ', name) - 1)
    data[i, "FirstName"] <- FirstName
    
    LastName <- substr(name, regexpr(' ', name) + 1, nchar(name))
    LastName <- sub(' Sr.', '', LastName)
    LastName <- sub(' Jr.', '', LastName)
    data[i, "LastName"] <- LastName
    
    if(AwayTeam == (data[i, "Team"])) {
      data[i, "Opponent"] <- HomeTeam
    } else {
      data[i, "Opponent"] <- AwayTeam
    }
  }
}