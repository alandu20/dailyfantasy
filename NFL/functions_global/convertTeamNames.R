if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for replace a vector of team names with the DK convention.


####### Function for Computing Covariance Matrix Given Start and End Date #######
convertTeamNames <- function(team_vec, from_source, to_source) {
  
  if (from_source=="DK" & to_source=="RG") {
    from_team_names <- c("SEA", "KC", "ARI", "NYJ", "PHI", "HOU", "MIN", "CIN", "BUF", "NE", "ATL", "GB", "IND", "NYG", "BAL", "TEN", "TB", "CHI", "DAL", "DET", "LAC", "OAK", "JAX", "CLE", "NO", "MIA", "SF", "CAR", "DEN", "PIT", "LAR", "WAS")
    to_team_names <- c("SEA", "KCC", "ARI", "NYJ", "PHI", "HOU", "MIN", "CIN", "BUF", "NEP", "ATL", "GBP", "IND", "NYG", "BAL", "TEN", "TBB", "CHI", "DAL", "DET", "LAC", "OAK", "JAC", "CLE", "NOS", "MIA", "SFO", "CAR", "DEN", "PIT", "LAR", "WAS")
  } else if (from_source=="RG" & to_source=="DK") {
    from_team_names <- c("SEA", "KCC", "ARI", "NYJ", "PHI", "HOU", "MIN", "CIN", "BUF", "NEP", "ATL", "GBP", "IND", "NYG", "BAL", "TEN", "TBB", "CHI", "DAL", "DET", "LAC", "OAK", "JAC", "CLE", "NOS", "MIA", "SFO", "CAR", "DEN", "PIT", "LAR", "WAS")
    to_team_names <-c("SEA", "KC", "ARI", "NYJ", "PHI", "HOU", "MIN", "CIN", "BUF", "NE", "ATL", "GB", "IND", "NYG", "BAL", "TEN", "TB", "CHI", "DAL", "DET", "LAC", "OAK", "JAX", "CLE", "NO", "MIA", "SF", "CAR", "DEN", "PIT", "LAR", "WAS")
  } else {
    stop("From and To Team Name Sources Not Found.")
  }
  
  for (i in 1:length(team_vec)) {
    team_vec[i] <- to_team_names[which(from_team_names==team_vec[i])]
  }
  
  return(team_vec)
}

