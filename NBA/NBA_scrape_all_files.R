

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


### Save DFS Directory Path

original_wd <- getwd()

### Load all Scraping Functions

setwd('scrapingContestData')
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)

setwd(original_wd)

### Read in Contest File
contest_info <- read.csv(file = 'NBA/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)


# Scrape DK Site for todays contests
days_ahead <- 1 # default 1
contest_info <- download_DK_daily_contests_NBA(contest_info, days_ahead=days_ahead)


### re-read in Contest File (to reindex)
contest_info <- read.csv(file = 'NBA/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)

# Find Earliest index of "yesterday's" contests
date_results <- Sys.Date()-1 # default Sys.Date()-1
first_contest_update <- min(which(as.Date(contest_info$Contest_Date) == date_results))
# first_contest_update <- 539

### Update Files

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

for(index in first_contest_update:length(contest_info$Contest_Date)) {
  # Case: Contest Occured Yesterday 
  if (as.Date(contest_info$Contest_Date[index]) == date_results) {
    print(paste0(index, ' of ', length(contest_info$Contest_Date), ' | Currently: ', contest_info$Contest_Name[index]))
    
    # get name
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)

    #Load in Player Results
    download_player_results_NBA('NBA', as.Date(contest_info$Contest_Date[index]))
    
    #Download DK Results File 
    download_DK_contest_file_NBA(contest_info$Contest_ID[index], 
                                 as.Date(contest_info$Contest_Date[index]),
                                 contest_name)
    
  } else if (as.Date(contest_info$Contest_Date[index]) == (Sys.Date()+days_ahead)) {
    print(paste0(index, ' of ', length(contest_info$Contest_Date), ' | Currently: ', contest_info$Contest_Name[index]))

    # get name
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)

    # Load in DK Salary Files
    download_DK_player_salary_file_NBA(contest_df = contest_info,
                                       ind = index)

    # Download DK Payout Structure
    download_DK_payout_structure_NBA(contest_number = contest_info$Contest_ID[index],
                                     date = as.Date(contest_info$Contest_Date[index]),
                                     contest_name = contest_name)
  }
}

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


### Download Rotogrinders Projections
print('Downloading Rotogrinders Projections')
download_rotogrinders_projections_NBA(Sys.Date())
