library(shiny)

setwd("~/Projects/DFS")
for (i in c(1:9, 11:15, 17)) {
  name <- paste0("contest_1M_results_wk", i)
  assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
  print(i)
}

analyzePros <- function(username) {
  buyIn <- c(3,rep(20,8), NA, rep(27,5), NA, 20)
  wks.20 <- c(2:9,17) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks) # hard coded
  wks.27 <- c(11:15)
  
  returnDataFrame <- as.data.frame(matrix(0,17,5))
  names(returnDataFrame) <- c("Week", "NumberLineups", "MaxScores", "BestPlace", "PnL")
  
  returnDataFrame$Week <- c(1:17)
  
  for (week in 1:17) {
    
    # Check if we have the data
    if(!exists(paste0("contest_1M_results_wk", week))) {
      returnDataFrame[week,c(2:5)] <- NA
      
    } else {
      #Load Payout Structure
      file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$", buyIn[week], "_payout_structure_week", week, ".csv")
      payout.data <- read.csv(file = file.name, stringsAsFactors = F)
      
      # Find Number of Lineups 
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", week)))
      temp.user.results <- temp.results[temp.results$User.Name==username,]
      
      
      returnDataFrame$NumberLineups[week] <- length(temp.user.results[,1])
      
      if(returnDataFrame$NumberLineups[week] == 0) {
        returnDataFrame[week,c(3:5)] <- NA
      } else {
        
        
        #Calculate MaxScores
        returnDataFrame$MaxScores[week] <- max(temp.user.results$Points)
        
        # Best Place
        returnDataFrame$BestPlace[week] <- min(temp.user.results$Rank)
        
        # Week PnL
        temp_PnL <- -(buyIn[week]*length(temp.user.results[,1]))
        for(lineup in 1:length(temp.user.results[,1])) {
          for (j in 1:nrow(payout.data)) {
            if (temp.user.results$Rank[lineup] >= payout.data$Place_lo[j] && temp.user.results$Rank[lineup] <= payout.data$Place_hi[j]) {
              temp_PnL <- temp_PnL + payout.data$Payout[j]
              break
            }
          }
        }
        
        returnDataFrame$PnL[week] <- temp_PnL
      }
    }
  }
  
  return(returnDataFrame)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  username <- renderText({ input$username })
  df <- reactiveValues(data = NULL)
  df <- analyzePros(username)
  
  output$distPlot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    if (is.null(v$data)) return()
    plot(df$week, df$PnL)
  })
})