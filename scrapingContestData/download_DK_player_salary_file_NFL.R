library(dplyr)
library(rvest)

  
download_DK_player_salary_file_NFL <- function(contest_info, index) {
  original_wd <- getwd()
  
  contest_number <- contest_info$Contest_ID[index]
  entryFee <- contest_info$Entry_Fee[index]
  date <- contest_info$Contest_Date[index]
  eventName <- contest_info$Contest_Name[index]
  
  url <- paste0('https://www.draftkings.com/contest/draftteam/', contest_number)
  thepage = readLines(url)
  
  html_block <- grep('draftGroupId\":', thepage, value=TRUE)[2]
  
  #Clean draftGroupId
  draftGroupId_start <- gregexpr("draftGroupId\"", html_block, fixed = TRUE)[[1]][1]
  draftGroupId <- substring(html_block,draftGroupId_start,nchar(html_block))
  draftGroupId <- substring(draftGroupId, 15, gregexpr(',', draftGroupId)[[1]][1] - 1)
  
  #Clean contestTypeId 
  contestTypeId_start <- gregexpr("contestTypeId\"", html_block, fixed = TRUE)[[1]][1]
  contestTypeId <- substring(html_block,contestTypeId_start,nchar(html_block))
  contestTypeId <- substring(contestTypeId, 16, gregexpr(',', contestTypeId)[[1]][1] - 1)
  
  
  
  
  browseURL(paste0('https://www.draftkings.com/lineup/getavailableplayerscsv?contestTypeId=', contestTypeId, "&draftGroupId=", draftGroupId))
  Sys.sleep(5)
  
  setwd('~/Downloads')
  
  while(!file.exists("DKSalaries.csv")){
    Sys.sleep(1)
  }
  
  player_salaries <- read.csv("DKSalaries.csv", stringsAsFactors = F)
  file.remove("DKSalaries.csv")

  data_warehouse_path <- file.path(original_wd,'NFL/data_warehouse')
  
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


