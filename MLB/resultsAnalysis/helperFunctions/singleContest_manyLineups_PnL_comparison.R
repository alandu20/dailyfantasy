


singleContest_manyLineups_PnL_comparison <- function(contest_info_row_number, regex_input = '*.csv', count_lineups=150) {
  
  
  original_wd <- getwd()
  
  #Load Helper Function
  source('MLB/resultsAnalysis/helperFunctions/compute_lineup_fpts.R')
  
  #Read in Contest Info
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
  contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)
  
  # Go to Desired Contest Directory
  base_contest_path = paste0("MLB/data_warehouse/", contest_info$Contest_Date[contest_info_row_number], "/" , 
                             paste0(contest_info$Entry_Fee[contest_info_row_number],"entry_",
                                    gsub(" ", "", contest_info$Contest_Name[contest_info_row_number])))
  setwd(base_contest_path)
  
  #Load in needed files
  contest_standings <- read.csv('contest-standings.csv', stringsAsFactors = F)
  payout_structure <- read.csv('payout_structure.csv', stringsAsFactors = F)
  player_performance <- read.csv('../player_results.csv', stringsAsFactors = F)
  hitters_input <- read.csv('hitters.csv', stringsAsFactors = F)
  pitchers_input <- read.csv('pitchers.csv', stringsAsFactors = F)
  # Go to Lineups Folder
  setwd('lineups')
  
  if (grepl(".csv", regex_input, fixed=T)) {
    file_names = regex_input
  } else {
    file_names = list.files(pattern = regex_input)
  }
  lineups = lapply(file_names, read.csv, stringsAsFactors = F)
  
  if (grepl(".csv", regex_input, fixed=T)) {
    lineups[[1]] <- lineups[[1]][1:count_lineups,]
  }
  
  setwd(original_wd)
  
  results <- as.data.frame(matrix(0,length(file_names),5))
  names(results) <- c('Name', 'PnL', 'Lineups', 'Lineups_Projections', 'Lineups_positions')
  results$Name <- file_names
  
  if (length(file_names) != 0) {
    for (counter in 1:length(file_names)) {
      if (counter %% 10 == 0) {
        print(paste0("Working on Lineup ", counter, " of ", length(file_names)))
      }
      
      lineup <- lineups[[counter]]
      
      output <- compute_lineup_fpts(player_performance, payout_structure, lineup, contest_info$Entry_Fee[contest_info_row_number], 
                                    contest_standings)
      PnL <- output[[2]]
      results$PnL[counter] <- PnL
      
      results$Lineups[counter] <- list(output[[1]])
      #print(paste0("PnL: ", PnL, "| Counter = ", counter, " | Lineup: ", temp[counter]))
      
      #Build Lineups Projections
      projection_lineup <- output[[1]]
      for(row in 1:10) {
        if(row <= 2) {
          projection_lineup[,row] <- paste0(pitchers_input$Projection_dfn[match(projection_lineup[,row],pitchers_input$Name)], " / ", pitchers_input$Actual_fpts[match(projection_lineup[,row],pitchers_input$Name)])
        } else {
          projection_lineup[,row] <- paste0(hitters_input$Projection_dfn[match(projection_lineup[,row],hitters_input$Name)], " / ", hitters_input$Actual_fpts[match(projection_lineup[,row],hitters_input$Name)])
        }
      }
      # projection_lineup$projected_total <- rowSums(projection_lineup[,c(1:10)])
      results$Lineups_Projections[counter] <- list(projection_lineup)
      
      
      #Build Lineups Position
      position_lineup <- output[[1]]
      for(row in 1:10) {
        if(row <= 2) {
          position_lineup[,row] <- paste0(pitchers_input$teamAbbrev[match(position_lineup[,row],pitchers_input$Name)])
        } else {
          position_lineup[,row] <- paste0(hitters_input$teamAbbrev[match(position_lineup[,row],hitters_input$Name)], "_", hitters_input$Batting_Order_Confirmed[match(position_lineup[,row],hitters_input$Name)])
        }
      }
      results$Lineups_positions[counter] <- list(position_lineup)
    } 
  } else {
    results <- NULL
  }
  
  return(results)
  
}