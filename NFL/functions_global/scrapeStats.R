if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for scraping completions, targets, completion %, TDs, and target % for every player
# listed on nflsavant.com. Rolling stats are computed for completions, targets, and TDs. We also assign
# three separate integer rankings to each player (based on rolling completions, rolling targets, and rolling
# TDs) within each team. This ranking will be minimized in the objective function of our integer program
# solved in julia.


scrapeStats <- function(date) {
  ####### IMPORT LIBRARIES #########
  require('rvest')
  require('stringr')

  ####### SET PARAMETERS #######
  yr <- substr(date, 1, 4)
  if (yr == "2016") {
    week.latest <- ceiling((as.numeric(date) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
  } else if (yr == "2017") {
    week.latest <- ceiling((as.numeric(date) - as.numeric(as.Date("2017-09-07")))/7 + 1) - 1
  } else {
    stop("Year not found.")
  }
  pos <- '' # should always be ""
  
  
  # Scrape
  ####### SET TEAM NAMES FOLLOWING NFL SAVANT NAMING CONVENTION #########
  team.names <- c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN','DET','GB','HOU','IND','JAX','KC',
                  'MIA','MIN','NE','NO','NYG','NYJ','OAK','PHI','PIT','SD','SEA','SF','LA','TB','TEN','WAS')
  
  player.names <- c() # initialize (will store all player names)
  
  ####### SCRAPE HTML TABLE FOR ALL TEAMS #########
  for (i in 1:week.latest) {
    stats.df <- data.frame(matrix(data = NA, nrow = 0, ncol = 9)) # initialize df to store targets data
    #colnames(stats.df) <- c('Rank', 'Name', 'Team', 'Pos', 'Completions', 'Targets', 'Comp.Pct', 'TDs', 'Target.Pct')
    
    # iterate through all teams
    for (j in 1:length(team.names)) {
      url <- paste0("http://nflsavant.com/targets.php?ddlTeam=", team.names[j], "&ddlYear=", yr, "&week=", i, "&rz=all&ddlPosition=", pos)
      temp.df <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="tblTargetsTotal"]') %>%
        html_table()
      temp.df <- temp.df[[1]]
      stats.df <- rbind(stats.df, temp.df)
    }
    
    # add week number
    stats.df$Week.Num <- i
    
    # assign variable name (with week number) to stats.df
    name <- paste("stats.df.wk", i, sep = "")
    assign(name, stats.df)
    
    #player.names <- c(player.names, stats.df$Name)
    #player.helper <- c(player.helper, paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))
    player.names <- c(player.names, paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.)) # prevents double counting / missing players with same name
    print(paste0('Week ', i, ' done'))
  }
  
  # Set unique names
  player.names <- unique(player.names)
  
  # Completions df
  completions.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
  completions.weekly[,1] <- player.names
  for (i in 2:(week.latest+1)) {
    stats.df <- eval(parse(text=paste("stats.df.wk", i-1, sep = "")))
    completions.weekly[,i] <- stats.df$Completions[match(completions.weekly[,1], paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))]
    completions.weekly[is.na(completions.weekly[,i]),i] <- 0
  }
  
  # Targets df
  targets.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
  targets.weekly[,1] <- player.names
  for (i in 2:(week.latest+1)) {
    stats.df <- eval(parse(text=paste("stats.df.wk", i-1, sep = "")))
    targets.weekly[,i] <- stats.df$Targets[match(targets.weekly[,1], paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))]
    targets.weekly[is.na(targets.weekly[,i]),i] <- 0
  }
  
  # TDs df
  TDs.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
  TDs.weekly[,1] <- player.names
  for (i in 2:(week.latest+1)) {
    stats.df <- eval(parse(text=paste("stats.df.wk", i-1, sep = "")))
    TDs.weekly[,i] <- stats.df$TDs[match(TDs.weekly[,1], paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))]
    TDs.weekly[is.na(TDs.weekly[,i]),i] <- 0
  }
  
  # Compute rolling stats for Completions, Targets, TDs, and Target %
  for (i in 1:week.latest) {
    # Initialize df
    temp.df <- as.data.frame(matrix(data = NA, nrow = length(player.names), ncol = 4))
    colnames(temp.df) <- c('Name','Completions.Rolling','Targets.Rolling','TDs.Rolling')
    temp.df$Name <- player.names
    
    # Compute Rolling Completions
    if (i==1) {
      temp.df$Completions.Rolling <- completions.weekly[,i+1]
    } else {
      temp.df$Completions.Rolling <- rowSums(completions.weekly[,2:(i+1)])
    }
    
    # Compute Rolling Targets
    if (i==1) {
      temp.df$Targets.Rolling <- targets.weekly[,i+1]
    } else {
      temp.df$Targets.Rolling <- rowSums(targets.weekly[,2:(i+1)])
    }
    
    # Compute Rolling TDs
    if (i==1) {
      temp.df$TDs.Rolling <- TDs.weekly[,i+1]
    } else {
      temp.df$TDs.Rolling <- rowSums(TDs.weekly[,2:(i+1)])
    }
    
    # split Name column into Name, Team, Position
    name.team.pos <- str_split_fixed(temp.df$Name, "@", 3) # split at @ symbol
    temp.df$Name <- name.team.pos[,1]
    temp.df$Team <- name.team.pos[,2]
    temp.df$Pos <- name.team.pos[,3]
    
    # Compute Rolling Target %
    temp.agg <-  data.frame(temp.df$Team, temp.df$Targets.Rolling)
    temp.agg <- aggregate(temp.agg[,-c(1)], by = list(temp.agg$temp.df.Team), FUN = sum)
    temp.df$Targets.Rolling.Sum <- temp.agg$x[match(temp.df$Team, temp.agg$Group.1)]
    temp.df$Target.Ptcg.Rolling <- temp.df$Targets.Rolling/temp.df$Targets.Rolling.Sum
    temp.df$Targets.Rolling.Sum <- NULL
    
    # assign variable name (with week number) to stats.df
    assign(paste0("rolling.stats.wk",i), temp.df)
  }
  
    # Compute Rolling Target %
    # i <- 1
    # temp.wk <- eval(parse(text=paste0("rolling.stats.wk",i)))
    # temp.agg <-  data.frame(temp.wk$Team, temp.wk$Targets.Rolling)
    # temp.agg <- aggregate(temp.agg[,-c(1)], by = list(temp.agg$temp.wk.Team), FUN = sum)
    # temp.wk$Targets.Rolling.Sum <- temp.agg$x[match(temp.wk$Team, temp.agg$Group.1)]
    # temp.wk$Target.Ptcg.Rolling <- temp.wk$Targets.Rolling/temp.wk$Targets.Rolling.Sum
    # temp.wk$Targets.Rolling.Sum <- NULL
    # assign(paste0("rolling.stats.wk",i), temp.wk)
    
    ####### ASSIGN INTEGER RANKING TO EACH PLAYER WITHIN TEAMS #########
    # TODO: factor in Redzone Targets, Completion %, etc
    for (i in 1:week.latest) {
      temp.wk <- eval(parse(text=paste0("rolling.stats.wk",i)))
      
      # add three ranking columns based on completions, targets, TDs (for ties, assign min rank)
      temp.wk <- transform(temp.wk, Rank.Completions = ave(temp.wk$Completions.Rolling, temp.wk$Team, FUN = function(x) rank(-x, ties.method = "min")))
      temp.wk <- transform(temp.wk, Rank.Targets = ave(temp.wk$Targets.Rolling, temp.wk$Team, FUN = function(x) rank(-x, ties.method = "min")))
      temp.wk <- transform(temp.wk, Rank.TDs = ave(temp.wk$TDs.Rolling, temp.wk$Team, FUN = function(x) rank(-x, ties.method = "min")))
      
      assign(paste0("rolling.stats.wk",i), temp.wk)
    }
    
    
    ####### WRITE TO FILE #########
    # for (i in 1:week.latest) {
    i <- week.latest
    if (write.bool==T) {
      write.csv(eval(parse(text=paste0("rolling.stats.wk",i))), file = paste0('optimizationCode/data_warehouse/stats/rolling.stats.wk',i,'.csv'), row.names = F)  
    }
    # }
  
  ####### APPEND TO 2016_cleaned_input FILES (only run after current week's data is prepared) #########
  # for (i in 2:week.latest) { # change to week.latest+1 once current week's data has been scraped
  i <- week.latest + 1 # make sure current week's data has been prepared already
  
  if (slate.days=="thu-mon") {
    temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/includes_thu-mon/offensive_players.csv'), stringsAsFactors = F) 
  } else if (slate.days=="sun-mon") {
    temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/includes_sun-mon/offensive_players.csv'), stringsAsFactors = F) 
  } else {
    temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), stringsAsFactors = F) 
  }
  
  # temp name cleaning
  temp$Temp.Name <- paste0(temp$LastName, ', ', temp$FirstName)
  temp$Temp.Name <- sub("'", "", temp$Temp.Name)
  
  # eval
  temp.rolling.wk <- eval(parse(text=paste0("rolling.stats.wk",i-1))) # i-1 b/c we use previous weeks rolling stats
  
  # add target rank
  temp$RankTargets <- temp.rolling.wk$Rank.Targets[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  temp$RankTargets[is.na(temp$RankTargets)==T] <- 0
  
  # add rolling target %
  temp$RollingTargetPctg <- temp.rolling.wk$Target.Ptcg.Rolling[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  temp$RollingTargetPctg[is.na(temp$RollingTargetPctg)==T] <- 0
  
  temp$Temp.Name <- NULL
  
  if (write.bool==T) {
    if (slate.days=="thu-mon") {
      write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/includes_thu-mon/offensive_players.csv'), row.names = F) 
    } else if (slate.days=="sun-mon") {
      write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/includes_sun-mon/offensive_players.csv'), row.names = F) 
    } else {
      write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), row.names = F) 
    } 
  }
  # }
  
  
  ####### ADD TO 2016_CLEANED_INPUT/ALL_DATA FILES (only run after current week's data is prepared) #########
  # for (i in 2:week.latest) {
  i <- week.latest + 1
  
  if (slate.days=="thu-mon") {
    temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/includes_thu-mon/offensive_players.csv'), stringsAsFactors = F)
  } else if (slate.days=="sun-mon") {
    temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/includes_sun-mon/offensive_players.csv'), stringsAsFactors = F)
  } else {
    temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), stringsAsFactors = F)
  }
  
  # temp name cleaning
  temp$Temp.Name <- paste0(temp$LastName, ', ', temp$FirstName)
  temp$Temp.Name <- sub("'", "", temp$Temp.Name)
  
  # eval
  temp.rolling.wk <- eval(parse(text=paste0("rolling.stats.wk",i-1))) # i-1 b/c we use previous weeks rolling stats
  
  # add Completions.Rolling, Targets.Rolling, TDs.Rolling, Rank.Completions, Rank.TDs
  temp$CompletionsRolling <- temp.rolling.wk$Completions.Rolling[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  temp$TargetsRolling <- temp.rolling.wk$Targets.Rolling[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  temp$TDsRolling <- temp.rolling.wk$TDs.Rolling[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  temp$RankCompletions <- temp.rolling.wk$Rank.Completions[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  temp$RankTDs <- temp.rolling.wk$Rank.TDs[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  
  # re-add Rank.Targets and Target.Ptcg.Rolling (but this time don't replace NAs with 0s)
  temp$RankTargets <- temp.rolling.wk$Rank.Targets[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  temp$RollingTargetPctg <- temp.rolling.wk$Target.Ptcg.Rolling[match(paste0(temp$Temp.Name,temp$Position), paste0(temp.rolling.wk$Name,temp.rolling.wk$Pos))]
  
  temp$Temp.Name <- NULL
  
  # write to file
  if (write.bool==T) {
    if (slate.days=="thu-mon") {
      write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/includes_thu-mon/offensive_players.csv'), row.names = F)
    } else if (slate.days=="sun-mon") {
      write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/includes_sun-mon/offensive_players.csv'), row.names = F)
    } else {
      write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), row.names = F)
    } 
  }
  # } 
}