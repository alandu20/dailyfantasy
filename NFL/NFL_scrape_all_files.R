# TODO:
# - download contest results section

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

sunday_date = as.Date("2017-12-31") # hard coded date

### Save DFS Directory Path

original_wd <- getwd()

### Load all Scraping Functions

setwd('scrapingContestData')
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)

setwd(original_wd)

### Read in Contest File
contest_info <- read.csv(file = 'NFL/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)


# Scrape DK Site for todays contests 
contest_info <- download_DK_daily_contests_NFL(contest_info = contest_info, sunday_date = sunday_date)

# remove arcade mode and pick'em contests
inds_arcade <- NULL
for (i in which(contest_info$Contest_Date==sunday_date)) {
  if (grepl("arcade", contest_info$Contest_Name[i], ignore.case = T) | grepl("pick'em", contest_info$Contest_Name[i], ignore.case = T)) {
    inds_arcade <- c(inds_arcade, i)
  }
}
if (!is.null(inds_arcade)) {
  contest_info <- contest_info[-c(inds_arcade),] 
  write.csv(contest_info, file = 'NFL/data_warehouse/contests.csv', row.names = F)
}

### re-read in Contest File (reset row index from 1)
contest_info <- read.csv(file = 'NFL/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)

write.csv(contest_info, file = 'NFL/data_warehouse/contests.csv', row.names = F)
# Find Earliest index of last contest

first_contest_update <- min(which(as.Date(contest_info$Contest_Date) >= sunday_date - 7))
# first_contest_update <- 777

# Index of 
for(index in first_contest_update:length(contest_info$Contest_Date)) {
  print(paste0(index, ' of ', length(contest_info$Contest_Date), ' | Currently: ', contest_info$Contest_Name[index]))
  # Case: Contest Occured Yesterday 
  if (as.Date(contest_info$Contest_Date[index]) < sunday_date) {
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)
    
    
    setwd(paste0('NFL/data_warehouse/', as.Date(contest_info$Contest_Date[index])))
    file.sources = list.files()
    cleaned_files <- toupper(file.sources)
    cleaned_files <- gsub(" ", "", cleaned_files, fixed = TRUE)
    
    folder <- grep(toupper(get('contest_name')), cleaned_files, value=TRUE)
    
    correct_index = 0  
    for(inner_index in 1:length(cleaned_files)) {
      if(strEndsWith(cleaned_files[inner_index], toupper(get('contest_name')))) {
        correct_index = inner_index
      }
    }
    
    setwd(original_wd)
    
    if (correct_index != 0) { 
      #Download DK Results File
      download_DK_contest_file_NFL(contest_info$Contest_ID[index],
                                   as.Date(contest_info$Contest_Date[index]),
                                   contest_name)
    }
    
    #Load in Player Results
    download_player_results('NFL', as.Date(contest_info$Contest_Date[index]))

    
    
  } else if (as.Date(contest_info$Contest_Date[index]) >= (Sys.Date())) {
    # CASE: If contest Occurs Today
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)
    
    # Load in DK Salary Files
    download_DK_player_salary_file_NFL(contest_info, index)

    
    # Download DK Payout Structure
    download_DK_payout_structure_NFL(contest_number = contest_info$Contest_ID[index], 
                                     date = as.Date(contest_info$Contest_Date[index]),
                                     contest_name = contest_name)
  }
}

### Clean DK Salary File
# TODO: probably add this to download_DK_player_salary_file.R
# source("NFL/functions_global/cleanDKSalaries.R")
# cleanDKSalaries(data = NULL)


### Download Rotogrinders Projections
print('Downloading Rotogrinders Projections')
download_rotogrinders_projections_NFL(sunday_date)


