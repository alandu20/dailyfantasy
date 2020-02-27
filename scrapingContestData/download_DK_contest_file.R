#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

download_DK_contest_file <- function(contest_number, entrance_fee, week_num) {
  original_wd <- getwd()
  browseURL(paste0('https://www.draftkings.com/contest/exportfullstandingscsv/', contest_number))
  setwd('~/Downloads')
  unzip(paste0("contest-standings-", contest_number, ".zip"))
  
  contest <- read.csv(paste0("contest-standings-", contest_number, ".csv"), stringsAsFactors = F)
  setwd(original_wd)
  
  write.csv(contest, file = paste0('resultsAnalysis/data_warehouse/contest_results/$', entrance_fee, '_full_results_week', week_num, '.csv'), row.names = F)
}


