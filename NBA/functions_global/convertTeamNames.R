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
    from_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GS","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NYK","NO","ORL","OKC","PHI","PHO","POR","TOR","SAC","SA","UTA","WAS") 
    to_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NYK","NOP","ORL","OKC","PHI","PHO","POR","TOR","SAC","SAS","UTA","WAS")
  } else if (from_source=="RG" & to_source=="DK") {
    from_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NOP","NYK","ORL","OKC","PHI","PHO","POR","TOR","SAC","SAS","UTA","WAS")
    to_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GS","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NO","NYK","ORL","OKC","PHI","PHO","POR","TOR","SAC","SA","UTA","WAS") 
  } else if (from_source=="DK" & to_source=="FC") {
    from_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GS","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NYK","NO","ORL","OKC","PHI","PHO","POR","TOR","SAC","SA","UTA","WAS")
    to_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NYK","NOP","ORL","OKC","PHI","PHX","POR","TOR","SAC","SAS","UTA","WAS")
  } else if (from_source=="FC" & to_source=="DK") {
    from_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NOP","NYK","ORL","OKC","PHI","PHX","POR","TOR","SAC","SAS","UTA","WAS") 
    to_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GS","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NO","NYK","ORL","OKC","PHI","PHO","POR","TOR","SAC","SA","UTA","WAS")
  } else if (from_source=="DK" & to_source=="DFN") {
    from_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GS","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NYK","NO","ORL","OKC","PHI","PHO","POR","TOR","SAC","SA","UTA","WAS")
    to_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NYK","NOR","ORL","OKC","PHI","PHO","POR","TOR","SAC","SAS","UTA","WAS")
  } else if (from_source=="DFN" & to_source=="DK") {
    from_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NOR","NYK","ORL","OKC","PHI","PHO","POR","TOR","SAC","SAS","UTA","WAS") 
    to_team_names <- c("ATL","BKN","BOS","CLE","CHA","CHI","DAL","DEN","DET","GS","HOU","IND","LAC","LAL","MEM","MIA","MIN","MIL","NO","NYK","ORL","OKC","PHI","PHO","POR","TOR","SAC","SA","UTA","WAS")
  }
  else {
    stop("From and To Team Name Sources Not Found.")
  }
  
  for (i in 1:length(team_vec)) {
    if (team_vec[i] %in% from_team_names) {
      team_vec[i] <- to_team_names[which(from_team_names==team_vec[i])]
    } else {
      print(paste0(team_vec[i], " is missing from convertTeamNames.R"))
    }
  }
  
  return(team_vec)
}