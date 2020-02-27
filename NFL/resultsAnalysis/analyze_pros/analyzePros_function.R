#setwd("~/Projects/DFS")
#load("cleaned_2016_results.Rdata")

analyzePros <- function(username) {
  buyIn <- c(3,rep(20,8), 44, rep(27,5), 50, 20)
  wks.20 <- c(2:9,17) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks) # hard coded
  wks.27 <- c(11:15)
  
  returnDataFrame <- as.data.frame(matrix(0,17,5))
  names(returnDataFrame) <- c("Week", "NumberLineups", "MaxScores", "BestPlace", "PnL")
  
  returnDataFrame$Week <- c(1:17)
  
  for (week in 1:17) {
    
    #file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$", buyIn[week], "_payout_structure_week", week, ".csv")
    
    # Check if we have the data
    if(!exists(paste0("contest_1M_results_wk", week))) {
      returnDataFrame[week,c(2:5)] <- NA
      
    } 
    # else if(!file.exists(file.name)) {
    #   returnDataFrame[week,c(2:5)] <- NA
    #   # Find Number of Lineups 
    #   temp.results <- eval(parse(text=paste0("contest_1M_results_wk", week)))
    #   temp.user.results <- temp.results[temp.results$User.Name==username,]
    #   
    #   
    #   returnDataFrame$NumberLineups[week] <- length(temp.user.results[,1])
    #   
    #   if(returnDataFrame$NumberLineups[week] == 0) {
    #     returnDataFrame[week,c(3:5)] <- NA
    #   } else {
    #     
    #     
    #     #Calculate MaxScores
    #     returnDataFrame$MaxScores[week] <- max(temp.user.results$Points)
    #     
    #     # Best Place
    #     returnDataFrame$BestPlace[week] <- min(temp.user.results$Rank)
    #   }
    # }
    else {
      #Load Payout Structure
      #payout.data <- read.csv(file = file.name, stringsAsFactors = F)
      
      payout.data <- eval(parse(text=paste0("payout_wk", week)))
      
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
  
  plot(returnDataFrame$Week, returnDataFrame$PnL, main = username)
  lines(returnDataFrame$Week, returnDataFrame$PnL)
  abline(h = 0, col = "red")
  
  
  return(returnDataFrame)
}





# ---- Graph Player Results  ---- #


graphPlayersResult <- function(username) {
  df <- analyzePros(username)
  
  cumsumCalc <- df$PnL
  cumsumCalc[is.na(cumsumCalc)]<-0
  
  ggplot(df,
         aes(y = PnL, x = Week)) +
    geom_point(aes(color = PnL>0), size = 1.7) + 
    geom_abline() + 
    geom_line(aes(x = Week, y = cumsum(cumsumCalc))) + 
    scale_color_manual(values=c("#cc0000", "#00CC00")) + labs(title=paste0(username, "'s 2016 Milly Maker Results")) +
    geom_text_repel(aes(label=PnL), size = 3) + 
    annotate("text", x = 18, y = cumsum(cumsumCalc)[17] , label = cumsum(cumsumCalc)[17])
  
}

username = "youdacao"
graphPlayersResult(username)

### Graph Multiple players at the same time. 

winner1 <- "SaahilSud"
winner2 <- "youdacao"
winner3 <- "CONDIA"
winner4 <- "aejones"
winner5 <- "CSURAM88"
winner6 <- "ehafner"
winner7 <- "BrandonAdams"
winner8 <- "Bales"
winner9 <- "00oreo00"
winner10 <- "ThatStunna"

temp <- as.data.frame(matrix(0,17,11))
names(temp) <- c("Week", winner1, winner2, winner3, winner4, winner5, winner6, winner7, winner8, winner9, winner10)
temp$Week <- 1:17

for (i in 1:10) {
  df <- analyzePros(names(temp)[i+1])
  
  cumsumCalc <- df$PnL
  cumsumCalc[is.na(cumsumCalc)]<-0
  temp[,i+1] <- cumsum(cumsumCalc)
}



ggplot(temp) +
  geom_line(aes(y = SaahilSud, x = Week, color = "SaahilSud")) +
  geom_line(aes(y = CONDIA, x = Week, color = "CONDIA")) +
  geom_line(aes(y = aejones, x = Week, color = "aejones")) +
  geom_line(aes(y = CSURAM88, x = Week, color = "CSURAM88")) + 
  geom_line(aes(y = ehafner, x = Week, color = "ehafner")) +
  geom_line(aes(y = BrandonAdams, x = Week, color = "BrandonAdams")) +
  geom_line(aes(y = youdacao, x = Week, color = "youdacao")) + 
  geom_line(aes(y = Bales, x = Week, color = "Bales")) +
  geom_line(aes(y = `00oreo00`, x = Week, color = "00oreo00")) +
  scale_colour_brewer(palette = "Set1") +
  geom_abline() + 
  labs(title=paste0("Pros 2016 Milly Maker Results")) +
  ylab("Profit")