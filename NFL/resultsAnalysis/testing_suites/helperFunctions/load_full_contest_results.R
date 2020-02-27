####### DESCRIPTION #########
# This file loads the full contest results into R
# Special notes for contest.entry.fee: for week 9, use $4 in lieu of $3 and for week 10, $27 in lieu of $20
# If the file does not exist, returns 0


load_full_contest_results <- function(week_number, contest_entry_fee, slate, year) {
  if (slate == 'main_slate') {
    if (contest_entry_fee == '$3') {
      if (week_number != 10) {
        file_name <- paste0("resultsAnalysis/data_warehouse/contest_results/", year, "/", slate, "/",
                            contest_entry_fee, "_contest_full_results_week", week_number, 
                            ".csv")
      } else{
        file_name <- paste0("resultsAnalysis/data_warehouse/contest_results/", year, "/", slate, "/",
                            '$4', "_contest_full_results_week", week_number, ".csv")
      }
      # Milly Maker / $20 Contests
    } else { 
      if (week_number > 9 && week_number != 17) {
        file_name <- paste0("resultsAnalysis/data_warehouse/contest_results/", year, "/", slate, "/",
                            '$27', "_contest_full_results_week", week_number, ".csv")
      } else {
        file_name <- paste0("resultsAnalysis/data_warehouse/contest_results/", year, "/", slate, "/",
                            contest_entry_fee, "_contest_full_results_week", week_number, ".csv")
      }
    }
  } else if (slate == 'thu-mon') {
    # } else if (thu_mon.bool==F & contest.entry.fee=='$20' & week.num > 9) {
    #   full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", 
    #                                               '$27', "_contest_full_results_week", week.num, ".csv"), 
    #                                 stringsAsFactors = F)
    # } else if (thu_mon.bool==T & contest.entry.fee=='$4') {
    #   full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/", 
    #                                               '$4', "_contest_full_results_week", week.num, ".csv"), 
    #                                 stringsAsFactors = F)
    
  }
  
  # Check that the file exists. 
  if(!file.exists(file_name)) { 
    print("***** FILE NOT FOUND *****")
    print(paste0('week: ', week_number, ' || contest_entry_fee: ', contest_entry_fee, ' || slate: ', slate, ' || year: ', year))
    print(paste0("File Path Attempted: /", file_name))
    return(0)
  } 
  
  return(read.csv(file = file_name, stringsAsFactors = F))
}

