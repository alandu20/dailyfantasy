#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we clean various csv files in the optimizationCode folder for ease of use in other scripts.
# TODO: clean up this hard coded shit

####### REMOVE FIRST AND LAST NAME FROM PLAYER PRODUCTION CSV #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  # load files
  name <- paste("draftkings_player_production_week", i, sep = "")
  assign(name, read.csv(file = paste0('resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week', i, '.csv'), stringsAsFactors = F))
  
  # remove Sr. and Jr.
  temp <- eval(parse(text=name))
  temp$Player <- sub(' Sr.', '', temp$Player)
  temp$Player <- sub(' Jr.', '', temp$Player)
  
  # write to file
  write.csv(temp, file = paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", i, ".csv"), row.names = F)
}