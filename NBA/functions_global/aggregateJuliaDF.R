if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Aggregates the following data into a dataframe:
# - projections (sources: Rotogrinders, DFN)
# - actual fpts (source: DFN)
#
# TODO:
# - match by player and position and team (not just player name)


aggregateJuliaDF <- function(contest.date, contest.name) {
  ####### Import Libraries #######
  require(stringr)
  
  ####### Import Functions #######
  source("NBA/functions_global/cleanPlayerNames.R")
  source("NBA/functions_global/convertTeamNames.R")
  
  ####### Load and Clean DK Salaries #######
  # load
  temp.dksalaries <- read.csv(file = paste0("NBA/data_warehouse/", contest.date,"/", contest.name, "/DKSalaries.csv"), stringsAsFactors = F, header = T)
  
  # clean
  temp.dksalaries$Name <- cleanPlayerNames(temp.dksalaries$Name)
  
  # fix teamAbbrev vs TeamAbbrev discrepancy that showed up in 2017 season
  temp_ind <- which(toupper(colnames(temp.dksalaries))=='TEAMABBREV')
  temp <- colnames(temp.dksalaries)[temp_ind]
  colnames(temp.dksalaries)[temp_ind] <- paste(tolower(substr(temp, 1, 1)), substr(temp, 2, nchar(temp)), sep="")
  
  # add opponent column
  temp.dksalaries$Temp_Team1 <- str_split_fixed(str_split_fixed(temp.dksalaries$GameInfo, " ", 2)[,1], "@", 2)[,1]
  temp.dksalaries$Temp_Team2 <- str_split_fixed(str_split_fixed(temp.dksalaries$GameInfo, " ", 2)[,1], "@", 2)[,2]
  for (i in 1:nrow(temp.dksalaries)) {
    if (temp.dksalaries$teamAbbrev[i]==temp.dksalaries$Temp_Team1[i]) {
      temp.dksalaries$Opponent[i] <- temp.dksalaries$Temp_Team2[i]
    } else {
      temp.dksalaries$Opponent[i] <- temp.dksalaries$Temp_Team1[i]
    }
  }
  temp.dksalaries$Temp_Team1 <- NULL
  temp.dksalaries$Temp_Team2 <- NULL
  
  # change team names to uppercase to normalize with other naming conventions
  temp.dksalaries$GameInfo <- toupper(temp.dksalaries$GameInfo)
  temp.dksalaries$teamAbbrev <- toupper(temp.dksalaries$teamAbbrev)
  temp.dksalaries$Opponent <- toupper(temp.dksalaries$Opponent)
  
  
  ####### Load projection Sources, Clean, and Add to DKSalaries DF #######
  # projection file paths
  path.rotogrinders <- paste0("NBA/data_warehouse/projections/rotogrinders/roto_", contest.date, ".csv")
  path.dfn <- paste0("NBA/data_warehouse/projections/dailyfantasynerd/dfn_", contest.date, ".csv")
  
  # rotogrinders
  if (file.exists(path.rotogrinders)) {
    temp.rotogrinders <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
    if (is.null(temp.rotogrinders$player)) {
      print(paste0("Rotogrinders (offense) headers incorrect. ", contest.date))
    }
    temp.rotogrinders$player <- cleanPlayerNames(temp.rotogrinders$player)
    temp.rotogrinders$team <- convertTeamNames(team_vec = temp.rotogrinders$team, from_source = "RG", to_source = "DK")
    temp.dksalaries$Projection <- temp.rotogrinders$fpts[match(paste0(temp.dksalaries$Name, temp.dksalaries$Position, temp.dksalaries$teamAbbrev), paste0(temp.rotogrinders$player, temp.rotogrinders$pos, temp.rotogrinders$team))]
  } else {
    temp.dksalaries$Projection <- NA
    warning(paste0("Rotogrinders projections not found. ", contest.date))
  }
  
  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    if (is.null(temp.dfn$Player.Name)) {
      print(paste0("DFN header incorrect. ", contest.date))
    }
    temp.dfn$Player.Name <- cleanPlayerNames(temp.dfn$Player.Name)
    temp.dfn$Team <- convertTeamNames(team_vec = temp.dfn$Team, from_source = "DFN", to_source = "DK")
    temp.dksalaries$Projection_dfn <- temp.dfn$Proj.FP[match(paste0(temp.dksalaries$Name, temp.dksalaries$Position, temp.dksalaries$teamAbbrev), paste0(temp.dfn$Player.Name, temp.dfn$Pos, temp.dfn$Team))]
  } else {
    temp.dksalaries$Projection_dfn <- NA
    warning(paste0("DFN projections not found. ", contest.date))
  }
  
  
  ####### Add Actual Fpts Column #######
  path.actual <- paste0("NBA/data_warehouse/", contest.date, "/player_results.csv")
  if (file.exists(path.actual)) {
    temp.actual <- read.csv(path.actual, stringsAsFactors = F, header = T)
    temp.actual$Player <- cleanPlayerNames(temp.actual$Player)
    temp.actual$Team <- convertTeamNames(team_vec = temp.actual$Team, from_source = "FC", to_source = "DK")
    temp.dksalaries$Actual_fpts <- temp.actual$Score[match(paste0(temp.dksalaries$Name, temp.dksalaries$teamAbbrev), paste0(temp.actual$Player, temp.actual$Team))]
  } else {
    temp.dksalaries$Actual_fpts <- NA
  }
  
  
  ####### Rename teamAbbrev column #######
  colnames(temp.dksalaries)[which(colnames(temp.dksalaries)=="teamAbbrev")] <- "Team"
  
  # return
  return(temp.dksalaries)
}

