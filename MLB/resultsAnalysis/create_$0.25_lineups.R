if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

####### Description #######


####### Description #######
source("MLB/functions_global/findDuplicateContests.R")

# load all contests and add file name column
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$file <- paste0(contest_info$Entry_Fee, "entry_", gsub(" ", "", contest_info$Contest_Name))

# load baseline contests
baseline_contests <- read.csv(file = 'MLB/optimizationCode/baseline_contests.csv', stringsAsFactors = F)

# initializations
dates <- baseline_contests$Date
output_df <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 4, dimnames = list(NULL, c("Date","Contest_names","contest_row_index", "max_entry"))))

#
for (d in 4:length(dates)) {
  # subset by date
  temp_contest_info <- contest_info[contest_info$Contest_Date==as.Date(dates[d]),]
  
  # find duplicate contests (goal is to not run more lineups)
  temp_contest_info <- findDuplicateContests(contest_info = temp_contest_info)
  match_id <- temp_contest_info$Match_ID[baseline_contests$Contest_names[d] == temp_contest_info$file]
  temp_contest_info <- temp_contest_info[temp_contest_info$Match_ID == match_id, ]
  
  # remove the special entry contests
  if (length(which(grepl(pattern = "[*[0-9]x]", x = temp_contest_info$Contest_Name))) != 0) {
    temp_contest_info <- temp_contest_info[-which(grepl(pattern = "[*[0-9]x]", x = temp_contest_info$Contest_Name)),] 
  }
  
  
  # find $0.25 contest
  ind <- which(temp_contest_info$Entry_Fee == "$0.25")
  
  # keep larger max entry contest if multiple matches
  if (length(ind) > 1) {
    ind <- ind[which.max(temp_contest_info$Max_Entry[ind])]
  }
  
  # add row to output_df
  temp_date <- temp_contest_info$Contest_Date[ind]
  temp_contest_name <- temp_contest_info$file[ind]
  temp_contest_row_index <- which(temp_contest_info$file[ind] == contest_info$file & temp_contest_info$Contest_Date[ind] == contest_info$Contest_Date)
  temp_max_entry <- temp_contest_info$Max_Entry[ind]
  
  output_df <- rbind(output_df, cbind(temp_date, temp_contest_name, temp_contest_row_index, temp_max_entry))
}

# fix column names
colnames(output_df) <- c("Date","Contest_names","contest_row_index", "max_entry")

# remove dates where missing $0.25 contest results
missing_dates <- c("2017-05-21", "2017-05-23", "2017-06-13", "2017-06-18")
output_df <- output_df[-which(output_df$Date %in% missing_dates),]

# write to file
write.csv(output_df, file = "MLB/optimizationCode/baseline_contests_$0.25.csv", row.names = F)


####### Description #######
baseline_contests <- baseline_contests[which(baseline_contests$Date %in% output_df$Date),]

# be careful, this creates a lot of new files
for (i in 1:nrow(baseline_contests)) {
  # list file paths all generated lineups
  file_paths <- list.files(path = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/", baseline_contests$Contest_names[i], "/lineups"), pattern = "*.csv*")
  
  print(baseline_contests$Date[i])
  print(paste0("Num files: ", length(file_paths)))
  
  # create empty lineups folder if it doesn't exist
  dir.create(file.path("MLB/data_warehouse/", output_df$Date[i], "/", output_df$Contest_names[i], "/lineups/"), showWarnings = FALSE)
  
  # cut down to max_entry of $0.25 contest and write to $0.25 contest folder
  temp_max <- as.numeric(as.character(output_df$max_entry[i]))
  for (j in 1:length(file_paths)) {
    temp_csv <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/", baseline_contests$Contest_names[i], "/lineups/", file_paths[j]), stringsAsFactors = F, header = T, check.names = F)
    temp_csv <- temp_csv[1:min(temp_max, nrow(temp_csv)),]
    
    # write to $0.25 contest folder
    write.csv(temp_csv, file = paste0("MLB/data_warehouse/", output_df$Date[i], "/", output_df$Contest_names[i], "/lineups/", file_paths[j]), row.names = F)
  }
}

