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
  source("NFL/functions_global/cleanPlayerNames.R")
  source("NFL/functions_global/convertTeamNames.R")
  source("NFL/functions_global/addValueWR.R")
  
  ####### Load and Clean DK Salaries #######
  # load
  temp.dksalaries <- read.csv(file = paste0("NFL/data_warehouse/", contest.date,"/", contest.name, "/DKSalaries.csv"), stringsAsFactors = F, header = T)
  
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

  # split into offense and defense
  temp.dksalaries.offense <- temp.dksalaries[!(temp.dksalaries$Position %in% c("DST")), ]
  temp.dksalaries.defense <- temp.dksalaries[temp.dksalaries$Position %in% c("DST"), ]
  remove(temp.dksalaries) # don't need original df anymore
  
  
  ####### Load projection Sources, Clean, and Add to DKSalaries DF (Offense) #######
  # projection file paths
  path.rotogrinders <- paste0("NFL/data_warehouse/projections/rotogrinders/roto_offense_", contest.date, ".csv")
  path.dfn <- paste0("NFL/data_warehouse/projections/dailyfantasynerd/dfn_offense_", contest.date, ".csv")
  
  # rotogrinders
  if (file.exists(path.rotogrinders)) {
    temp.rotogrinders.offense <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
    if (is.null(temp.rotogrinders.offense$player)) {
      print(paste0("Rotogrinders (offense) headers incorrect. ", contest.date))
    }
    temp.rotogrinders.offense$player <- cleanPlayerNames(temp.rotogrinders.offense$player)
    temp.rotogrinders.offense$team <- convertTeamNames(team_vec = temp.rotogrinders.offense$team, from_source = "RG", to_source = "DK")
    temp.dksalaries.offense$Projection <- temp.rotogrinders.offense$fpts[match(paste0(temp.dksalaries.offense$Name, temp.dksalaries.offense$Position, temp.dksalaries.offense$teamAbbrev), paste0(temp.rotogrinders.offense$player, temp.rotogrinders.offense$pos, temp.rotogrinders.offense$team))]
  } else {
    temp.dksalaries.offense$Projection <- NA
    warning(paste0("Rotogrinders projections not found. ", contest.date))
  }
  
  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn.offense <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    if (is.null(temp.dfn.offense$Player.Name)) {
      print(paste0("DFN (hitters) headers incorrect. ", contest.date))
    }
    temp.dfn.offense$Player.Name <- cleanPlayerNames(temp.dfn.offense$Player.Name)
    temp.dksalaries.offense$Projection_dfn <- temp.dfn.offense$Proj.FP[match(paste0(temp.dksalaries.offense$Name, temp.dksalaries.offense$Position, temp.dksalaries.offense$teamAbbrev), paste0(temp.dfn.offense$Player.Name, temp.dfn.offense$Pos, temp.dfn.offense$Team))]
  } else {
    temp.dksalaries.offense$Projection_dfn <- NA
    warning(paste0("DFN projections not found. ", contest.date))
  }

  
  ####### Load projection Sources, Clean, and Add to DKSalaries DF (Defense) #######
  # projection file paths
  path.rotogrinders <- paste0("NFL/data_warehouse/projections/rotogrinders/roto_defense_", contest.date, ".csv")
  path.dfn <- paste0("NFL/data_warehouse/projections/dailyfantasynerd/dfn_defense_", contest.date, ".csv")

  # rotogrinders
  if (file.exists(path.rotogrinders)) {
    temp.rotogrinders.defense <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
    if (is.null(temp.rotogrinders.defense$player)) {
      print(paste0("Rotogrinders (pitchers) headers incorrect. ", contest.date))
    }
    temp.rotogrinders.defense$player <- cleanPlayerNames(temp.rotogrinders.defense$player)
    temp.rotogrinders.defense$team <- convertTeamNames(team_vec = temp.rotogrinders.defense$team, from_source = "RG", to_source = "DK")
    temp.dksalaries.defense$Projection <- temp.rotogrinders.defense$fpts[match(temp.dksalaries.defense$teamAbbrev, temp.rotogrinders.defense$team)]
  } else {
    temp.dksalaries.defense$Projection <- NA
    warning(paste0("Rotogrinders projections not found. ", contest.date))
  }

  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn.defense <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    if (is.null(temp.dfn.defense$Player.Name)) {
      print(paste0("DFN (pitchers) headers incorrect. ", contest.date))
    }
    temp.dfn.defense$Player.Name <- cleanPlayerNames(temp.dfn.defense$Player.Name)
    temp.dksalaries.defense$Projection_dfn <- temp.dfn.defense$Proj.FP[match(temp.dksalaries.defense$teamAbbrev, temp.dfn.defense$Team)]
  } else {
    temp.dksalaries.defense$Projection_dfn <- NA
    warning(paste0("DFN projections not found. ", contest.date))
  }

  ####### Add Actual Fpts Column #######
  # Offense (dfn)
  path.dfn.offense.actual <- paste0("NFL/data_warehouse/projections/dailyfantasynerd/updates/dfn_offense_", contest.date, ".csv")
  if (file.exists(path.dfn.offense.actual)) {
    temp.dfn.offense.actual <- read.csv(path.dfn.offense.actual, stringsAsFactors = F, header = T)
    temp.dfn.offense.actual$Player.Name <- cleanPlayerNames(temp.dfn.offense.actual$Player.Name)
    temp.dksalaries.offense$Actual_fpts <- temp.dfn.offense.actual$Actual.FP[match(paste0(temp.dksalaries.offense$Name, temp.dksalaries.offense$teamAbbrev), paste0(temp.dfn.offense.actual$Player.Name, temp.dfn.offense.actual$Team))]
  } else {
    temp.dksalaries.offense$Actual_fpts <- NA
  }

  # Defense (dfn)
  path.dfn.defense.actual <- paste0("NFL/data_warehouse/projections/dailyfantasynerd/updates/dfn_defense_", contest.date, ".csv")
  if (file.exists(path.dfn.defense.actual)) {
    temp.dfn.defense.actual <- read.csv(path.dfn.defense.actual, stringsAsFactors = F, header = T)
    temp.dfn.defense.actual$Player.Name <- cleanPlayerNames(temp.dfn.defense.actual$Player.Name)
    temp.dksalaries.defense$Actual_fpts <- temp.dfn.defense.actual$Actual.FP[match(temp.dksalaries.defense$teamAbbrev, temp.dfn.defense.actual$Team)]
  } else {
    temp.dksalaries.defense$Actual_fpts <- NA
  }

  # Hitters and Pitchers (fantasy cruncher)
  # path.actual.fpts <- paste0("NFL/data_warehouse/", contest.date,"/player_results.csv")
  # if (file.exists(path.actual.fpts)) {
  #   temp.actual.fpts <- read.csv(path.actual.fpts, stringsAsFactors = F, header = T)
  #   temp.actual.fpts$Player <- cleanPlayerNames(temp.actual.fpts$Player)
  #   temp.dksalaries.offense$Actual_fpts <- temp.actual.fpts$Actual.Score[match(temp.dksalaries.offense$Name, temp.actual.fpts$Player)]
  #   temp.dksalaries.defense$Actual_fpts <- temp.actual.fpts$Actual.Score[match(temp.dksalaries.defense$Name, temp.actual.fpts$Player)]
  #   if (sum(is.na(temp.dksalaries.defense$Actual_fpts))==length(temp.dksalaries.defense$Actual_fpts)) {
  #     warning(paste0("All Actual_fpts elements are NA.", contest.date))
  #   }
  # } else {
  #   temp.dksalaries.offense$Actual_fpts <- NA
  #   warning(paste0("Fantasy Cruncher player_results.csv not found.", contest.date))
  # }
  
  ####### Additional columns to offense #######
  # RankTargets
  temp.dksalaries.offense$RankTargets <- NA
  temp.dksalaries.offense$RollingTargetPctg <- NA
  temp.dksalaries.offense$FreqInd <- NA
  # temp.dksalaries.offense$ValueWR <- NA
  temp.dksalaries.offense$TierRank <- NA
  
  ####### ValueWR column #######
  temp.dksalaries.offense$ValueWR <- addValueWR(offense_data = temp.dksalaries.offense, max_salary = 5000)
  
  ####### Rename teamAbbrev column #######
  colnames(temp.dksalaries.offense)[which(colnames(temp.dksalaries.offense)=="teamAbbrev")] <- "Team"
  colnames(temp.dksalaries.defense)[which(colnames(temp.dksalaries.defense)=="teamAbbrev")] <- "Team"
  
  # return
  julia.inputs <- list(temp.dksalaries.offense, temp.dksalaries.defense)
  return(julia.inputs)
}

