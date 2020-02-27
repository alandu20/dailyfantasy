#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

library(dplyr)
library(rvest)


download_DK_player_salary_file <- function(contest_number, date) {
  original_wd <- getwd()
  
  url <- paste0('https://www.draftkings.com/contest/draftteam/', contest_number)
  thepage = readLines(url)
  
  #Clean draftGroupId
  draftGroupId <- grep('draftGroupId:', thepage, value=TRUE)
  draftGroupId <- gsub(" ", "", draftGroupId, fixed = TRUE)
  draftGroupId <- gsub(",", "", draftGroupId, fixed = TRUE)
  draftGroupId <- strsplit(draftGroupId, ":")[[1]][2]
  
  #Clean contestTypeId 
  contestTypeId <- grep('contestTypeId:', thepage, value=TRUE)
  contestTypeId <- gsub(" ", "", contestTypeId, fixed = TRUE)
  contestTypeId <- gsub(",", "", contestTypeId, fixed = TRUE)
  contestTypeId <- strsplit(contestTypeId, ":")[[1]][2]
  
  #Clean entryFee
  entryFee <- grep('Entry Fee - ', thepage, value=TRUE)
  entryFee <- gsub(" ", "", entryFee, fixed = TRUE)
  entryFee <- strsplit(entryFee, "-")[[1]][2]
  entryFee <- strsplit(entryFee, "<")[[1]][1]

  
  #Clean eventName
  eventName <- grep('event-name', thepage, value=TRUE)
  eventName <- gsub(" ", "", eventName, fixed = TRUE)
  eventName <- strsplit(eventName, ">")[[1]][2]
  eventName <- strsplit(eventName, "<")[[1]][1]
  eventName <- sub("&#39;", "'", eventName, fixed = TRUE)
  
  
  browseURL(paste0('https://www.draftkings.com/lineup/getavailableplayerscsv?contestTypeId=', contestTypeId, "&draftGroupId=", draftGroupId))
  Sys.sleep(5)
  
  setwd('~/Downloads')
  
  while(!file.exists("DKSalaries.csv")){
    Sys.sleep(1)
  }
  
  player_salaries <- read.csv("DKSalaries.csv", stringsAsFactors = F)
  file.remove("DKSalaries.csv")
  #setwd(paste0(original_wd, 'MLB/data_warehouse/', date)
  data_warehouse_path <- file.path(original_wd,'MLB/data_warehouse')
  
  setwd(data_warehouse_path)
  dir.create(file.path(date))
  setwd(as.character(date))
  
  eventDir <- paste0(entryFee, "entry_", eventName)  
  dir.create(eventDir)
  setwd(eventDir)  
        
  write.csv(player_salaries, file = 'DKSalaries.csv', row.names = F)
  
  dir.create(file.path('lineups')) # Create Directory for future testing lineups
  
  # Add a temp file so that 
  fileConn<-file("lineups/ignore.txt")
  writeLines(c("tempfile"), fileConn)
  close(fileConn)
  
  
  setwd(original_wd)
  
  return(0)
}


