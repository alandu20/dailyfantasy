

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
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)


# Scrape DK Site for todays contests 
contest_info <- download_DK_daily_contests_MLB(contest_info)

# remove arcade mode and pick'em contests
inds_arcade <- NULL
for (i in which(contest_info$Contest_Date==Sys.Date())) {
  if (grepl("arcade", contest_info$Contest_Name[i], ignore.case = T) | grepl("pick'em", contest_info$Contest_Name[i], ignore.case = T)) {
    inds_arcade <- c(inds_arcade, i)
  }
}
if (!is.null(inds_arcade)) {
  contest_info <- contest_info[-c(inds_arcade),] 
  write.csv(contest_info, file = 'MLB/data_warehouse/contests.csv', row.names = F)
}

### re-read in Contest File (not sure if needed but a safety precaution)
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)

# Find Earliest index of "yesterday's" contests
first_contest_update <- min(which(as.Date(contest_info$Contest_Date) == Sys.Date() - 1))

### Update Files

for(index in first_contest_update:length(contest_info$Contest_Date)) {
  print(paste0(index, ' of ', length(contest_info$Contest_Date), ' | Currently: ', contest_info$Contest_Name[index]))
  # Case: Contest Occured Yesterday 
  if (as.Date(contest_info$Contest_Date[index]) < (Sys.Date())) {
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)

    #Load in Player Results
    download_player_results('MLB', as.Date(contest_info$Contest_Date[index]))
    
    #Download DK Results File 
    download_DK_contest_file_MLB(contest_info$Contest_ID[index], 
                                 as.Date(contest_info$Contest_Date[index]),
                                 contest_name)
    
  } else if (as.Date(contest_info$Contest_Date[index]) == (Sys.Date())) {
  # CASE: If contest Occurs Today
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)
    
    # Load in DK Salary Files
    download_DK_player_salary_file(contest_info$Contest_ID[index], 
                                   as.Date(contest_info$Contest_Date[index]))
    
    # Download DK Payout Structure
    download_DK_payout_structure_MLB(contest_info$Contest_ID[index], 
                                     as.Date(contest_info$Contest_Date[index]),
                                     contest_name)
    
    
    
  }
  
  
}

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


# Download Yesterday's Updated DFN File
print('Download Yesterday\'s Updated DFN File')
download_dfn_update(Sys.Date()-1)

# Download Yesterday's Updated BBMonster Player Stats
print('Download Yesterday\'s Updated Player Stats')
#download_BBmonster_player_stats(Sys.Date()-1)

### Download Rotogrinders Projections
print('Downloading Rotogrinders Projections')
download_rotogrinders_projections_MLB(Sys.Date())

### Download DFN Projections
print('Downloading DFN Projections')
download_dfn_projections()

### Download BaseballMonster Projections
print('Downloading BaseballMonster Projections')
download_BBmonster_projections()

### Download FantasyPros Projections
#print('Downloading FantasyPros Projections')
#download_fantasypros_projections_MLB()

# Download Rotowire Projections
# NEED TO FINISH

### Create Julia Inputs (hitters.csv, pitchers.csv, covariance matrices)
source("MLB/MLB_create_julia_inputs.R")
MLB_create_julia_inputs(date_start = Sys.Date(), date_end = Sys.Date(), filter_names = c("chg75p_exp(spike)"))



#quit(save='no')