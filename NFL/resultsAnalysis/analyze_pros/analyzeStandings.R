#setwd("~/Projects/DFS")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we examine the standings (contest results) of the millionaire maker, specifically studying top users
# such as SaahilSud and ChipotleAddict. This file is segmented into sections.
#
# Dependencies: cleanContestResults.R
#
# SECTION LOAD. LOAD FILES
# SECTION I. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS THAT CASHED
# SECTION II. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS > SOME PERCENTILE
# SECTION III. EXAMINE NUMBER OF UNIQUE PLAYERS (OVERALL AND BY POSITION)
# SECTION IV. EXAMINE POSITION EXPOSURES (%)
#   - for each position, plots exposure to top 5 players (in terms of exposure among all lineups submitted by user for the week)
# SECTION V. EXAMINE POSITION EXPOSURES (COUNT)
# SECTION VI. COMPARE NUMBER OF UNIQUE PLAYERS TO OUR FORMULATIONS
# SECTION VII. EXAMINE SALARY DISTRIBUTION BY POSITION


####### SET SECTION TO RUN #######
section.run <- "7" # 1-6 or "LOAD" # run LOAD first, then any section! # if 6, run 5 first
subsection.run <- "2" # only used if section.run has a subsection


####### SET PARAMETERS #######
slate.days <- "" # "thu-mon" or "sun-mon" or ""
wks.20 <- c(2:9,17) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks) # hard coded
wks.27 <- c(11:15) # c(11:15) if using sunday only (if thu-mon or sun-mon, need to enter weeks) # hard coded
# week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
week.latest <- 17

user.name <- "ChipotleAddict" # SaahilSud, youdacao, scout326, ehafner, ChipotleAddict, Theclone, awesemo, AssaniFisher, aejones, CONDIA

pctls.vec <- c(0.75, 0.85, 0.95, 0.99) # percentile thresholds to plot (enter any 4 between 0.0-1.0) (for Section II)

num.games.sunday <- c(13,14,14,13,12,13,13,11,11,12,12,12,13,14,13,12) # up to wk 16 (for Section II) (note that milly maker is sunday only)


####### READ IN CLEANED RESULTS FILES #########
if (section.run=="LOAD") {
  for (i in c(wks.20, wks.27)) {
    name <- paste0("contest_1M_results_wk", i)
    if (slate.days=="thu-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
    } else if (slate.days=="sun-mon") {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
    } else {
      assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    }
    print(i)
  }
}


####### SECTION I. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS THAT CASHED #########
if (section.run==1) {
  temp.cashing <- read.csv(file = "resultsAnalysis/data_warehouse/weekly_payout_structure/contest_1M_cashing.csv", stringsAsFactors = F)
  cashing.pct.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 10, dimnames = list(NULL, c("Week","Pct.Cashing","Max.Fpts","Min.Fpts","Best.Place","Worst.Place","170-180","180-190","190-200","200+"))))
  for (i in 1:week.latest) {
    cashing.pct.mat[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      # Compute % of submitted lineups that cashed
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      cashing.pct.mat$Pct.Cashing[i] <- sum(temp.user.results$Points > temp.cashing$Min[temp.cashing$Week==i])/nrow(temp.user.results)
      
      # Max fpts
      cashing.pct.mat$Max.Fpts[i] <- max(temp.user.results$Points)
      
      # Min fpts
      cashing.pct.mat$Min.Fpts[i] <- min(temp.user.results$Points)
      
      # Best Place
      cashing.pct.mat$Best.Place[i] <- min(temp.user.results$Rank)
      
      # Worst Place
      cashing.pct.mat$Worst.Place[i] <- max(temp.user.results$Rank)
      
      # 180-190, 190-200, 200+
      cashing.pct.mat$`170-180`[i] <- sum(temp.user.results$Points > 170 & temp.user.results$Points <= 180)
      cashing.pct.mat$`180-190`[i] <- sum(temp.user.results$Points > 180 & temp.user.results$Points <= 190)
      cashing.pct.mat$`190-200`[i] <- sum(temp.user.results$Points > 190 & temp.user.results$Points <= 200)
      cashing.pct.mat$`200+`[i] <- sum(temp.user.results$Points > 200)
    }
  }
  plot(1:week.latest, cashing.pct.mat$Pct.Cashing, xlab = "Week", ylab = "% Cashing", main = paste0("Pct Lineups that Cash (MillyMaker): ", user.name), type = "b")
  plot(1:week.latest, cashing.pct.mat$Max.Fpts, xlab = "Week", ylab = "Fpts", main = paste0("Max Fantasy Points (MillyMaker): ", user.name), type = "b")
  # plot(1:week.latest, cashing.pct.mat$Min.Fpts, xlab = "Week", ylab = "Fpts", main = paste0("Min Fantasy Points (MillyMaker): ", user.name), type = "b")
  # plot(1:week.latest, cashing.pct.mat$Best.Place, xlab = "Week", ylab = "Place", main = paste0("Max Place (MillyMaker): ", user.name), type = "b")
}


####### SECTION II. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS > SOME PERCENTILE #########
if (section.run==2) {
  pctls.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 5, dimnames = list(NULL, c("Week", paste0(pctls.vec[1]*100,"pctle.Fpts"), paste0(pctls.vec[2]*100,"pctle.Fpts"), paste0(pctls.vec[3]*100,"pctle.Fpts"), paste0(pctls.vec[4]*100,"pctle.Fpts")))))
  for (i in 1:week.latest) {
    pctls.mat[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      # Compute % of submitted lineups that exceed fpts threshold for each percentile
      for (p in 1:length(pctls.vec)) {
        temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
        temp.user.results <- temp.results[temp.results$User.Name==user.name,]
        pctls.mat[i,p+1] <- sum(temp.user.results$Points > quantile(temp.results$Points, pctls.vec[p]))/nrow(temp.user.results) 
      }
    }
  }
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    plot(1:week.latest, pctls.mat[,i+1], xlab = "Week", ylab = "% Cashing", main = paste0("Pct Lineups > ",pctls.vec[i]*100,"th Pctle (MillyMaker): ",user.name), type = "b")
  }
  
  par(mfrow=c(1,1))
  num.games.sunday.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 2, dimnames = list(NULL, c("Week","num.games.sunday"))))
  num.games.sunday.mat[,1] <- 1:(week.latest)
  num.games.sunday.mat[,2] <- num.games.sunday
  plot(num.games.sunday.mat, type = 'b', main = "Number of Sunday Games")
}


####### SECTION III. EXAMINE NUMBER OF UNIQUE PLAYERS (OVERALL AND BY POSITION) #########
if (section.run==3) {
  num.unique.players.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 8, dimnames = list(NULL, c("Week","Num.Unique.All","Num.Unique.QB","Num.Unique.RB1-2","Num.Unique.WR1-3","Num.Unique.TE","Num.Unique.FLEX","Num.Unique.DST"))))
  for (i in 1:week.latest) {
    num.unique.players.mat[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      # Compute % of submitted lineups that cashed
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      num.unique.players.mat[i,2] <- length(unique(unlist(temp.user.results[,6:14])))
      num.unique.players.mat[i,3] <- length(unique(unlist(temp.user.results[,'QB'])))
      num.unique.players.mat[i,4] <- length(unique(unlist(temp.user.results[,c('RB1','RB2')])))
      num.unique.players.mat[i,5] <- length(unique(unlist(temp.user.results[,c('WR1','WR2','WR3')])))
      num.unique.players.mat[i,6] <- length(unique(unlist(temp.user.results[,c('TE')])))
      num.unique.players.mat[i,7] <- length(unique(unlist(temp.user.results[,c('FLEX')])))
      num.unique.players.mat[i,8] <- length(unique(unlist(temp.user.results[,c('DST')])))
    }
  }
  
  par(mfrow=c(1,1))
  plot(1:week.latest, num.unique.players.mat[,2], xlab = "Week", ylab = "Num Unique Players", main = paste0(colnames(num.unique.players.mat)[2], " (MillyMaker): ", user.name), type = "b") 
  print(paste0(colnames(num.unique.players.mat)[2], " correlation: ", cor(1:week.latest, num.unique.players.mat[,2], use = "complete.obs")))
  
  par(mfrow=c(3,2))
  for (i in 2:7) {
    plot(1:week.latest, num.unique.players.mat[,i+1], xlab = "Week", ylab = "Num Unique Players", main = paste0(colnames(num.unique.players.mat)[i+1], " (MillyMaker): ", user.name), type = "b") 
    print(paste0(colnames(num.unique.players.mat)[i+1], " correlation: ", cor(1:week.latest, num.unique.players.mat[,i+1], use = "complete.obs")))
  }
}


####### SECTION IV. EXAMINE POSITION EXPOSURES (%) #########
if (section.run==4) {
  qb.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","QB.Exposure.1","QB.Exposure.2","QB.Exposure.3","QB.Exposure.4","QB.Exposure.5"))))
  rb.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","RB.Exposure.1","RB.Exposure.2","RB.Exposure.3","RB.Exposure.4","RB.Exposure.5"))))
  wr.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","WR.Exposure.1","WR.Exposure.2","WR.Exposure.3","WR.Exposure.4","WR.Exposure.5"))))
  te.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","TE.Exposure.1","TE.Exposure.2","TE.Exposure.3","TE.Exposure.4","TE.Exposure.5"))))
  flex.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","FLEX.Exposure.1","FLEX.Exposure.2","FLEX.Exposure.3","FLEX.Exposure.4","FLEX.Exposure.5"))))
  dst.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","DST.Exposure.1","DST.Exposure.2","DST.Exposure.3","DST.Exposure.4","DST.Exposure.5"))))
  for (i in 1:week.latest) {
    qb.exposure.mat[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      temp.lineups <- temp.user.results[,6:14]
      
      occurences <- sort(table(unlist(temp.lineups[,c("QB")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      qb.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("RB1","RB2")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      rb.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("WR1","WR2","WR3")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      wr.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("TE")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      te.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("FLEX")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      flex.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("DST")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      dst.exposure.mat[i,2:6] <- exposure[1:5]
    }
  }
  
  par(mfrow=c(3,2))
  # plot qb exposure
  plot(1:week.latest, qb.exposure.mat[,2], ylim = c(min(qb.exposure.mat[,6], na.rm = T),max(qb.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 QB Exposure (MillyMaker)"), type = "b")
  points(qb.exposure.mat[,3], type = "b", col = 'red')
  points(qb.exposure.mat[,4], type = "b", col = 'blue')
  points(qb.exposure.mat[,5], type = "b", col = 'green')
  points(qb.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot rb exposure
  plot(1:week.latest, rb.exposure.mat[,2], ylim = c(min(rb.exposure.mat[,6], na.rm = T),max(rb.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 RB Exposure (MillyMaker)"), type = "b")
  points(rb.exposure.mat[,3], type = "b", col = 'red')
  points(rb.exposure.mat[,4], type = "b", col = 'blue')
  points(rb.exposure.mat[,5], type = "b", col = 'green')
  points(rb.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot wr exposure
  plot(1:week.latest, wr.exposure.mat[,2], ylim = c(min(wr.exposure.mat[,6], na.rm = T),max(wr.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 WR Exposure (MillyMaker)"), type = "b")
  points(wr.exposure.mat[,3], type = "b", col = 'red')
  points(wr.exposure.mat[,4], type = "b", col = 'blue')
  points(wr.exposure.mat[,5], type = "b", col = 'green')
  points(wr.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot te exposure
  plot(1:week.latest, te.exposure.mat[,2], ylim = c(min(te.exposure.mat[,6], na.rm = T),max(te.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 TE Exposure (MillyMaker)"), type = "b")
  points(te.exposure.mat[,3], type = "b", col = 'red')
  points(te.exposure.mat[,4], type = "b", col = 'blue')
  points(te.exposure.mat[,5], type = "b", col = 'green')
  points(te.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot flex exposure
  plot(1:week.latest, flex.exposure.mat[,2], ylim = c(min(flex.exposure.mat[,6], na.rm = T),max(flex.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 FLEX Exposure (MillyMaker)"), type = "b")
  points(flex.exposure.mat[,3], type = "b", col = 'red')
  points(flex.exposure.mat[,4], type = "b", col = 'blue')
  points(flex.exposure.mat[,5], type = "b", col = 'green')
  points(flex.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot DST exposure
  plot(1:week.latest, dst.exposure.mat[,2], ylim = c(min(dst.exposure.mat[,6], na.rm = T),max(dst.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 DST Exposure (MillyMaker)"), type = "b")
  points(dst.exposure.mat[,3], type = "b", col = 'red')
  points(dst.exposure.mat[,4], type = "b", col = 'blue')
  points(dst.exposure.mat[,5], type = "b", col = 'green')
  points(dst.exposure.mat[,6], type = "b", col = 'yellow')
}


####### SECTION V. EXAMINE POSITION EXPOSURES (COUNTS) #########
if (section.run==5) {
  total.count <- rep(NA, week.latest)
  qb.count <- rep(NA, week.latest)
  rb.count <- rep(NA, week.latest)
  wr.count <- rep(NA, week.latest)
  te.count <- rep(NA, week.latest)
  flex.count <- rep(NA, week.latest)
  dst.count <- rep(NA, week.latest)
  for (i in 1:week.latest) {
    if (i %in% c(wks.20, wks.27)) {
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      temp.lineups <- temp.user.results[,6:14]
      
      occurences <- sort(table(unlist(temp.lineups)), decreasing=T)
      total.count[i] <- length(occurences)
      
      occurences <- sort(table(unlist(temp.lineups[,c("QB")])), decreasing=T)
      qb.count[i] <- length(occurences)
      
      occurences <- sort(table(unlist(temp.lineups[,c("RB1","RB2")])), decreasing=T)
      rb.count[i] <- length(occurences)
      
      occurences <- sort(table(unlist(temp.lineups[,c("WR1","WR2","WR3")])), decreasing=T)
      wr.count[i] <- length(occurences)
      
      occurences <- sort(table(unlist(temp.lineups[,c("TE")])), decreasing=T)
      te.count[i] <- length(occurences)
      
      occurences <- sort(table(unlist(temp.lineups[,c("FLEX")])), decreasing=T)
      flex.count[i] <- length(occurences)
      
      occurences <- sort(table(unlist(temp.lineups[,c("DST")])), decreasing=T)
      dst.count[i] <- length(occurences)
    }
  }
  
  par(mfrow=c(3,2))
  plot(1:week.latest, qb.count, ylim = c(min(qb.count, na.rm = T),max(qb.count, na.rm = T)), xlab = "Week", ylab = "Count", main = paste0(user.name, "'s QB Count (MillyMaker)"), type = "b")
  plot(1:week.latest, rb.count, ylim = c(min(rb.count, na.rm = T),max(rb.count, na.rm = T)), xlab = "Week", ylab = "Count", main = paste0(user.name, "'s RB Count (MillyMaker)"), type = "b")
  plot(1:week.latest, wr.count, ylim = c(min(wr.count, na.rm = T),max(wr.count, na.rm = T)), xlab = "Week", ylab = "Count", main = paste0(user.name, "'s WR Count (MillyMaker)"), type = "b")
  plot(1:week.latest, te.count, ylim = c(min(te.count, na.rm = T),max(te.count, na.rm = T)), xlab = "Week", ylab = "Count", main = paste0(user.name, "'s TE Count (MillyMaker)"), type = "b")
  plot(1:week.latest, flex.count, ylim = c(min(flex.count, na.rm = T),max(flex.count, na.rm = T)), xlab = "Week", ylab = "Count", main = paste0(user.name, "'s FLEX Count (MillyMaker)"), type = "b")
  plot(1:week.latest, dst.count, ylim = c(min(dst.count, na.rm = T),max(dst.count, na.rm = T)), xlab = "Week", ylab = "Count", main = paste0(user.name, "'s DST Count (MillyMaker)"), type = "b")
  
  par(mfrow=c(1,1))
  plot(1:week.latest, total.count, ylim = c(min(total.count, na.rm = T),max(total.count, na.rm = T)), xlab = "Week", ylab = "Count", main = paste0(user.name, "'s Total Player Count (MillyMaker)"), type = "b") # plot total count
}


####### SECTION VI. COMPARE NUMBER OF UNIQUE PLAYERS TO OUR FORMULATIONS #########
if (section.run==6) {
  
  if (subsection.run==1) {
    #------ Formulation 4 ------#
    # overlap = 4
    # exposure = 0.4
    
    total.us.count <- rep(NA, week.latest)
    qb.us.count <- rep(NA, week.latest)
    rb.us.count <- rep(NA, week.latest)
    wr.us.count <- rep(NA, week.latest)
    te.us.count <- rep(NA, week.latest)
    flex.us.count <- rep(NA, week.latest)
    dst.us.count <- rep(NA, week.latest)
    for (i in 1:week.latest) {
      if (i %in% c(wks.20, wks.27)) {
        temp.lineups <- read.csv(file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", i, "_dfn_formulation4_overlap_4_exposure_0.4.csv"))
        
        occurences <- sort(table(unlist(temp.lineups)), decreasing=T)
        total.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("QB")])), decreasing=T)
        qb.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("RB","RB.1")])), decreasing=T)
        rb.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("WR","WR.1","WR.2")])), decreasing=T)
        wr.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("TE")])), decreasing=T)
        te.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("FLEX")])), decreasing=T)
        flex.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("DST")])), decreasing=T)
        dst.us.count[i] <- length(occurences)
      }
    }
    
    par(mfrow=c(3,2))
    plot(1:week.latest, qb.us.count, ylim = c(min(c(qb.us.count,qb.count), na.rm = T),max(c(qb.us.count,qb.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 4 (b) vs ", user.name, " (r) QB Count (MillyMaker)"), type = "b")
    points(qb.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, rb.us.count, ylim = c(min(c(rb.us.count,rb.count), na.rm = T),max(c(rb.us.count,rb.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 4 (b) vs ", user.name, " (r) RB Count (MillyMaker)"), type = "b")
    points(rb.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, wr.us.count, ylim = c(min(c(wr.us.count,wr.count), na.rm = T),max(c(wr.us.count,wr.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 4 (b) vs ", user.name, " (r) WR Count (MillyMaker)"), type = "b")
    points(wr.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, te.us.count, ylim = c(min(c(te.us.count,te.count), na.rm = T),max(c(te.us.count,te.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 4 (b) vs ", user.name, " (r) TE Count (MillyMaker)"), type = "b")
    points(te.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, flex.us.count, ylim = c(min(c(flex.us.count,flex.count), na.rm = T),max(c(flex.us.count,flex.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 4 (b) vs ", user.name, " (r) FLEX Count (MillyMaker)"), type = "b")
    points(flex.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, dst.us.count, ylim = c(min(c(dst.us.count,dst.count), na.rm = T),max(c(dst.us.count,dst.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 4 (b) vs ", user.name, " (r) DST Count (MillyMaker)"), type = "b")
    points(dst.count, type = "b", col = 'red') # other user's
    
    par(mfrow=c(1,1))
    plot(1:week.latest, total.us.count, ylim = c(min(c(total.us.count,total.count), na.rm = T),max(c(total.us.count,total.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 4 (b) vs ", user.name, " (r) Total Player Count (MillyMaker)"), type = "b") # plot total count
    points(total.count, type = "b", col = 'red') # other user's
  }
  
  
  if (subsection.run==2) {
    #------ Formulation 14 ------#
    # overlap = 4
    # exposure_defense = 0.25
    # exposure_wr = 0.25
    # exposure_rb = 0.75
    # exposure_te = 0.75
    # exposure_qb = 0.5
    
    total.us.count <- rep(NA, week.latest)
    qb.us.count <- rep(NA, week.latest)
    rb.us.count <- rep(NA, week.latest)
    wr.us.count <- rep(NA, week.latest)
    te.us.count <- rep(NA, week.latest)
    flex.us.count <- rep(NA, week.latest)
    dst.us.count <- rep(NA, week.latest)
    for (i in 1:week.latest) {
      if (i %in% c(wks.20, wks.27)) {
        temp.lineups <- read.csv(file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", i, "_dfn_formulation14_overlap_4_defexp_0.25_wrexp_0.25_rbexp_0.75_teexp_0.75_qbexp_0.5.csv"))
        
        occurences <- sort(table(unlist(temp.lineups)), decreasing=T)
        total.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("QB")])), decreasing=T)
        qb.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("RB","RB.1")])), decreasing=T)
        rb.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("WR","WR.1","WR.2")])), decreasing=T)
        wr.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("TE")])), decreasing=T)
        te.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("FLEX")])), decreasing=T)
        flex.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("DST")])), decreasing=T)
        dst.us.count[i] <- length(occurences)
      }
    }
    
    par(mfrow=c(3,2))
    plot(1:week.latest, qb.us.count, ylim = c(min(c(qb.us.count,qb.count), na.rm = T),max(c(qb.us.count,qb.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 14 (b) vs ", user.name, " (r) QB Count (MillyMaker)"), type = "b")
    points(qb.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, rb.us.count, ylim = c(min(c(rb.us.count,rb.count), na.rm = T),max(c(rb.us.count,rb.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 14 (b) vs ", user.name, " (r) RB Count (MillyMaker)"), type = "b")
    points(rb.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, wr.us.count, ylim = c(min(c(wr.us.count,wr.count), na.rm = T),max(c(wr.us.count,wr.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 14 (b) vs ", user.name, " (r) WR Count (MillyMaker)"), type = "b")
    points(wr.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, te.us.count, ylim = c(min(c(te.us.count,te.count), na.rm = T),max(c(te.us.count,te.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 14 (b) vs ", user.name, " (r) TE Count (MillyMaker)"), type = "b")
    points(te.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, flex.us.count, ylim = c(min(c(flex.us.count,flex.count), na.rm = T),max(c(flex.us.count,flex.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 14 (b) vs ", user.name, " (r) FLEX Count (MillyMaker)"), type = "b")
    points(flex.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, dst.us.count, ylim = c(min(c(dst.us.count,dst.count), na.rm = T),max(c(dst.us.count,dst.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 14 (b) vs ", user.name, " (r) DST Count (MillyMaker)"), type = "b")
    points(dst.count, type = "b", col = 'red') # other user's
    
    par(mfrow=c(1,1))
    plot(1:week.latest, total.us.count, ylim = c(min(c(total.us.count,total.count), na.rm = T),max(c(total.us.count,total.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 14 (b) vs ", user.name, " (r) Total Player Count (MillyMaker)"), type = "b") # plot total count
    points(total.count, type = "b", col = 'red') # other user's
  }
  
  
  if (subsection.run==3) {
    #------ Formulation 15 ------#
    # overlap = 4
    # exposure_defense = 0.25
    # exposure_wr = 0.25
    # exposure_rb = 0.75
    # exposure_te = 0.75
    # exposure_qb = 0.5
    
    total.us.count <- rep(NA, week.latest)
    qb.us.count <- rep(NA, week.latest)
    rb.us.count <- rep(NA, week.latest)
    wr.us.count <- rep(NA, week.latest)
    te.us.count <- rep(NA, week.latest)
    flex.us.count <- rep(NA, week.latest)
    dst.us.count <- rep(NA, week.latest)
    for (i in 1:week.latest) {
      if (i %in% c(wks.20, wks.27) & i > 3) { # form 15 only feasible for week >= 4
        temp.lineups <- read.csv(file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", i, "_dfn_formulation15_overlap_4_defexp_0.25_wrexp_0.25_rbexp_0.75_teexp_0.75_qbexp_0.5_valuewrexp_0.15.csv"))
        
        occurences <- sort(table(unlist(temp.lineups)), decreasing=T)
        total.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("QB")])), decreasing=T)
        qb.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("RB","RB.1")])), decreasing=T)
        rb.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("WR","WR.1","WR.2")])), decreasing=T)
        wr.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("TE")])), decreasing=T)
        te.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("FLEX")])), decreasing=T)
        flex.us.count[i] <- length(occurences)
        
        occurences <- sort(table(unlist(temp.lineups[,c("DST")])), decreasing=T)
        dst.us.count[i] <- length(occurences)
      }
    }
    
    par(mfrow=c(3,2))
    plot(1:week.latest, qb.us.count, ylim = c(min(c(qb.us.count,qb.count), na.rm = T),max(c(qb.us.count,qb.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 15 (b) vs ", user.name, " (r) QB Count (MillyMaker)"), type = "b")
    points(qb.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, rb.us.count, ylim = c(min(c(rb.us.count,rb.count), na.rm = T),max(c(rb.us.count,rb.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 15 (b) vs ", user.name, " (r) RB Count (MillyMaker)"), type = "b")
    points(rb.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, wr.us.count, ylim = c(min(c(wr.us.count,wr.count), na.rm = T),max(c(wr.us.count,wr.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 15 (b) vs ", user.name, " (r) WR Count (MillyMaker)"), type = "b")
    points(wr.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, te.us.count, ylim = c(min(c(te.us.count,te.count), na.rm = T),max(c(te.us.count,te.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 15 (b) vs ", user.name, " (r) TE Count (MillyMaker)"), type = "b")
    points(te.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, flex.us.count, ylim = c(min(c(flex.us.count,flex.count), na.rm = T),max(c(flex.us.count,flex.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 15 (b) vs ", user.name, " (r) FLEX Count (MillyMaker)"), type = "b")
    points(flex.count, type = "b", col = 'red') # other user's
    plot(1:week.latest, dst.us.count, ylim = c(min(c(dst.us.count,dst.count), na.rm = T),max(c(dst.us.count,dst.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 15 (b) vs ", user.name, " (r) DST Count (MillyMaker)"), type = "b")
    points(dst.count, type = "b", col = 'red') # other user's
    
    par(mfrow=c(1,1))
    plot(1:week.latest, total.us.count, ylim = c(min(c(total.us.count,total.count), na.rm = T),max(c(total.us.count,total.count), na.rm = T)), xlab = "Week", ylab = "Count", main = paste0("Form 15 (b) vs ", user.name, " (r) Total Player Count (MillyMaker)"), type = "b") # plot total count
    points(total.count, type = "b", col = 'red') # other user's
  }
  
}


####### SECTION VII. EXAMINE SALARY DISTRIBUTION BY POSITION #########
if (section.run==7) {
  # par(mfrow=c(3,2))
  par(mfrow=c(2,2))
  for (i in 1:week.latest) {
    if (i %in% c(wks.20, wks.27) & i != 14) {
      # subset user's lineups
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      temp.lineups <- temp.user.results[,6:14]
      
      # load salaries for user
      temp.salaries <- read.csv(file = paste0("optimizationCode/data_warehouse/draftkings/DKSalaries_week", i, ".csv"), stringsAsFactors = F)
      temp.salaries$Name <- sub(' Sr.', '', temp.salaries$Name)
      temp.salaries$Name <- sub(' Jr.', '', temp.salaries$Name)
      
      # create df of salaries for user
      temp.user.salaries <- temp.lineups
      for (m in 1:nrow(temp.lineups)) {
        for (n in 1:ncol(temp.lineups)) {
          temp.user.salaries[m,n] <- sub(' Sr.', '', temp.user.salaries[m,n])
          temp.user.salaries[m,n] <- sub(' Jr.', '', temp.user.salaries[m,n])
          temp.user.salaries[m,n] <- as.numeric(temp.salaries$Salary[match(temp.user.salaries[m,n], temp.salaries$Name)])
        }
      }
      
      # load our formulation
      temp.us.lineups <- read.csv(file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", i, "_dfn_formulation14_overlap_4_defexp_0.25_wrexp_0.25_rbexp_0.75_teexp_0.75_qbexp_0.5.csv"), stringsAsFactors = F)
      
      # load salaries for our formulation
      temp.salaries$Name...ID <- as.character(temp.salaries$Name...ID) # need to use Name...ID
      
      # create df of salaries for our formulation
      temp.us.salaries <- temp.us.lineups
      for (m in 1:nrow(temp.us.lineups)) {
        for (n in 1:ncol(temp.us.lineups)) {
          temp.us.salaries[m,n] <- as.numeric(temp.salaries$Salary[match(temp.us.salaries[m,n], temp.salaries$Name...ID)])
        }
      }
      
      
      if (subsection.run==1) {
        #------ RBs ------#
        # sort user's RBs in decreasing order within lineup
        for (m in 1:nrow(temp.user.salaries)) {
          temp.user.salaries[m, c("RB1","RB2")] <- sort(c(as.numeric(temp.user.salaries$RB1[m]), as.numeric(temp.user.salaries$RB2[m])), decreasing = T)
          temp.us.salaries[m, c("RB","RB.1")] <- sort(c(as.numeric(temp.us.salaries$RB[m]), as.numeric(temp.us.salaries$RB.1[m])), decreasing = T)
        }
        
        temp.user.salaries <- temp.user.salaries[order(as.numeric(temp.user.salaries$RB1), decreasing = T),] # sort by RB1
        temp.us.salaries <- temp.us.salaries[order(as.numeric(temp.us.salaries$RB), decreasing = T),] # sort by RB1
        
        # Plot higher salary RB1 against lower salary RB2
        # plot(temp.user.salaries$RB1, temp.user.salaries$RB2, xlab = "RB1 (higher salary)", ylab = "RB2 (lower salary)", main = paste0("Week ", i, ", ", user.name)) # user
        # plot(temp.us.salaries$RB, temp.us.salaries$RB.1, xlab = "RB1 (higher salary)", ylab = "RB2 (lower salary)", main = paste0("Week ", i, ", Form 14")) # our formulation
        
        # RB count
        # length(unique(c(temp.lineups$RB1, temp.lineups$RB2))) # user
        # length(unique(c(temp.us.lineups$RB, temp.us.lineups$RB.1))) # our formulation 

        # stacked bar plot for user
        bar.data <- rbind(temp.user.salaries$RB1, temp.user.salaries$RB2)
        rownames(bar.data) <- c("RB1", "RB2")
        colnames(bar.data) <- 1:nrow(temp.user.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", ",user.name,", Salary Distribution RB1-RB2"), xlab="Lineup", ylab = "Salary", col=c("darkblue","red"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue","red"))
        
        # stacked bar plot for our formulation
        bar.data <- rbind(temp.us.salaries$RB, temp.us.salaries$RB.1)
        rownames(bar.data) <- c("RB1", "RB2")
        colnames(bar.data) <- 1:nrow(temp.us.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", Form 14, Salary Distribution WR1-WR3"), xlab="Lineup", ylab = "Salary", col=c("darkblue","red"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue","red"))
      }
      
      if (subsection.run==2) {
        #------ WRs ------#
        # sort user's WRs in decreasing order within lineup
        for (m in 1:nrow(temp.user.salaries)) {
          temp.user.salaries[m, c("WR1","WR2","WR3")] <- sort(c(as.numeric(temp.user.salaries$WR1[m]), as.numeric(temp.user.salaries$WR2[m]), as.numeric(temp.user.salaries$WR3[m])), decreasing = T)
          temp.us.salaries[m, c("WR","WR.1","WR.2")] <- sort(c(as.numeric(temp.us.salaries$WR[m]), as.numeric(temp.us.salaries$WR.1[m]), as.numeric(temp.us.salaries$WR.2[m])), decreasing = T)
        }
        
        temp.user.salaries <- temp.user.salaries[order(as.numeric(temp.user.salaries$WR1), decreasing = T),] # sort by WR1
        temp.us.salaries <- temp.us.salaries[order(as.numeric(temp.us.salaries$WR), decreasing = T),] # sort by WR1
        
        # stacked bar plot for user
        bar.data <- rbind(temp.user.salaries$WR1, temp.user.salaries$WR2, temp.user.salaries$WR3)
        rownames(bar.data) <- c("WR1", "WR2", "WR2")
        colnames(bar.data) <- 1:nrow(temp.user.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", ",user.name,", Salary Distribution WR1-WR3"), xlab="Lineup", ylab = "Salary", col=c("darkblue","red", "green"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue","red", "green"))
        
        # stacked bar plot for our formulation
        bar.data <- rbind(temp.us.salaries$WR, temp.us.salaries$WR.1, temp.us.salaries$WR.2)
        rownames(bar.data) <- c("WR1", "WR2", "WR2")
        colnames(bar.data) <- 1:nrow(temp.us.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", Form 14, Salary Distribution WR1-WR3"), xlab="Lineup", ylab = "Salary", col=c("darkblue","red", "green"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue","red", "green"))
      }
      
      # Form 14 spending more than 25k on WR1-WR3: wks 11, 8, 7, 4, 2
      # ChipotleAddict spending more than 25k on WR1-WR3: wks 6, 3
      
      # Form 14 spending more than 25k on WR1-WR3: wks 11, 8, 7, 4, 2
      # SaahilSud spending more than 25k on WR1-WR3: wks 7, 5
    
      
      if (subsection.run==3) {
        #------ QBs ------#
        temp.user.salaries <- temp.user.salaries[order(as.numeric(temp.user.salaries$QB), decreasing = T),] # sort by QB
        temp.us.salaries <- temp.us.salaries[order(as.numeric(temp.us.salaries$QB), decreasing = T),] # sort by QB
  
        # stacked bar plot for user
        bar.data <- rbind(temp.user.salaries$QB)
        rownames(bar.data) <- c("QB")
        colnames(bar.data) <- 1:nrow(temp.user.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", ",user.name,", Salary Distribution QB"), xlab="Lineup", ylab = "Salary", col=c("darkblue"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue"))
        
        # stacked bar plot for our formulation
        bar.data <- rbind(temp.us.salaries$QB)
        rownames(bar.data) <- c("QB")
        colnames(bar.data) <- 1:nrow(temp.us.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", Form 14, Salary Distribution QB"), xlab="Lineup", ylab = "Salary", col=c("darkblue"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue"))
      }
      
      
      if (subsection.run==4) {
        #------ TEs ------#
        temp.user.salaries <- temp.user.salaries[order(as.numeric(temp.user.salaries$TE), decreasing = T),] # sort by TE
        temp.us.salaries <- temp.us.salaries[order(as.numeric(temp.us.salaries$TE), decreasing = T),] # sort by TE
        
        # stacked bar plot for user
        bar.data <- rbind(temp.user.salaries$TE)
        rownames(bar.data) <- c("TE")
        colnames(bar.data) <- 1:nrow(temp.user.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", ",user.name,", Salary Distribution TE"), xlab="Lineup", ylab = "Salary", col=c("darkblue"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue"))
        
        # stacked bar plot for our formulation
        bar.data <- rbind(temp.us.salaries$TE)
        rownames(bar.data) <- c("TE")
        colnames(bar.data) <- 1:nrow(temp.us.salaries)
        bar.data <- as.table(bar.data, header = T)
        barplot(bar.data, main=paste0("Wk ",i,", Form 14, Salary Distribution TE"), xlab="Lineup", ylab = "Salary", col=c("darkblue"))
        legend("bottomleft", legend = rownames(bar.data), cex = 0.6, fill = c("darkblue"))
      }
      
      
      # # histogram of QB salaries
      # hist(as.numeric(temp.user.salaries$QB))
      # 
      # # histogram of total RB salaries
      # hist(as.numeric(temp.user.salaries$RB1) + as.numeric(temp.user.salaries$RB2))
      # 
      # # histogram of total WR salaries
      # hist(as.numeric(temp.user.salaries$WR1) + as.numeric(temp.user.salaries$WR2) + as.numeric(temp.user.salaries$WR3))
      # 
      # # histogram of TE salaries
      # hist(as.numeric(temp.user.salaries$TE))
      
      
      # # histogram of QB salaries
      # hist(as.numeric(temp.us.salaries$QB))
      # 
      # # histogram of total RB salaries
      # hist(as.numeric(temp.us.salaries$RB) + as.numeric(temp.us.salaries$RB.1))
      # 
      # # histogram of total WR salaries
      # hist(as.numeric(temp.us.salaries$WR) + as.numeric(temp.us.salaries$WR.1) + as.numeric(temp.us.salaries$WR.2))
      # 
      # # histogram of TE salaries
      # hist(as.numeric(temp.us.salaries$TE))      
    }
  }
}




