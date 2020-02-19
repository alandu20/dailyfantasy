if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for cleaning player names.


cleanPlayerNames <- function(df_name_column) {
  # remove Sr. and Jr.
  df_name_column <- sub(' Sr.', '', df_name_column)
  df_name_column <- sub(' Jr.', '', df_name_column)
  
  # remove middle initial and period
  df_name_column <- gsub(" [A-Z]\\. ", " ", df_name_column)
  
  # remove all periods
  df_name_column <- gsub("\\.", "", df_name_column)
  
  # replace common accented letters with english counterpart
  df_name_column <- gsub("á", "a", df_name_column)
  df_name_column <- gsub("é", "e", df_name_column)
  df_name_column <- gsub("í", "i", df_name_column)
  df_name_column <- gsub("ó", "o", df_name_column)
  df_name_column <- gsub("ú", "u", df_name_column)
  df_name_column <- gsub("ñ", "n", df_name_column)
  
  # edit specific player names (from, to, string)
  # N/A
  
  return(df_name_column)
}

