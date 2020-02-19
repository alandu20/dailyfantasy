#Author: Alan Du (aydu@princeton.edu) 

setwd("setwd("~/Desktop/Projects/FantasyFootball/correlationAnalysis")")
#save.image("corr.RData")
load("corr.RData")


######################################################## Results ###########################################################
View(avg.pairwise.corr.matrix)
View(median.pairwise.corr.matrix) # probably more useful than avg
View(max.pairwise.corr.matrix) # useful for getting an understanding of best-case worst-case but could be misleading b/c
# some players only have a few data points so the correlation between a pair of such players could be artificially high/low
View(min.pairwise.corr.matrix)
# Clear positive correlation between QB1-WR1, QB1-WR2, QB1-WR3. But WR1-WR2, WR1-WR3, WR2-WR3 all have slightly negative
# correlation. Slightly positive correlation between QB1-RB1, QB1-RB2, QB1-RB3. But clear negative correlation between
# RB1-RB2, RB1-RB3, and slightly negative correlation between RB2-RB3. Clear positive correlation between QB1-TE1. Slightly
# positive correlation between TE1-WR3. Perhaps one stack could be QB1-TE1-WR3. QB1-TE2-WR3 may also be a good stack.
############################################################################################################################


#--------- Preprocess data ---------#
data <- read.csv("2015_Corr.csv", header = F, stringsAsFactors = F)
for (i in 1:nrow(data)) {
  data[i,2:ncol(data)] <- as.numeric(data[i,2:ncol(data)]) 
}

qb <- 1
wr <- 1
rb <- 1
te <- 1

i <- 1
for (i in 1:nrow(data)) {
  if (data[i,1]=="QB") {
    data[i,1] <- paste0(as.character(data[i,1]), qb)
    qb <- qb + 1
  }
  
  else if (data[i,1]=="WR") {
    data[i,1] <- paste0(as.character(data[i,1]), wr)
    wr <- wr + 1
  }
  
  else if (data[i,1]=="RB") {
    data[i,1] <- paste0(as.character(data[i,1]), rb)
    rb <- rb + 1
  }
  
  else if (data[i,1]=="TE") {
    data[i,1] <- paste0(as.character(data[i,1]), te)
    te <- te + 1
  }
  
  else if (data[i,1]=="BREAK") {
    qb <- 1
    wr <- 1
    rb <- 1
    te <- 1
  }
}


#--------- Subset each position ---------#
# DraftKings: QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX (RB/WR/TE), and DST
QB1 <- which(data[,1] == "QB1")

WR1 <- which(data[,1] == "WR1")
WR2 <- which(data[,1] == "WR2")
WR3 <- which(data[,1] == "WR3")

RB1 <- which(data[,1] == "RB1")
RB2 <- which(data[,1] == "RB2")
RB3 <- which(data[,1] == "RB3")

TE1 <- which(data[,1] == "TE1")
TE2 <- which(data[,1] == "TE2")

DEF <- which(data[,1] == "DEF")

BREAK <- c(0, which(data[,1] == "BREAK")) # for computation purposes
num.teams <- length(QB1) # for computation purposes


#--------- Create matrix of average pairwise correlations for each pair of positions ---------#
pos.all <- cbind(QB1, WR1, WR2, WR3, RB1, RB2, RB3, TE1, TE2, DEF)
avg.pairwise.corr.matrix <- matrix(data=NA, nrow=ncol(pos.all), ncol=ncol(pos.all))
rownames(avg.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")
colnames(avg.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")

median.pairwise.corr.matrix <- matrix(data=NA, nrow=ncol(pos.all), ncol=ncol(pos.all))
rownames(median.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")
colnames(median.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")

max.pairwise.corr.matrix <- matrix(data=NA, nrow=ncol(pos.all), ncol=ncol(pos.all)) # for seeing how many NAs were removed
rownames(max.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")
colnames(max.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")

min.pairwise.corr.matrix <- matrix(data=NA, nrow=ncol(pos.all), ncol=ncol(pos.all)) # for seeing how many NAs were removed
rownames(min.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")
colnames(min.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")

numteams.pairwise.corr.matrix <- matrix(data=NA, nrow=ncol(pos.all), ncol=ncol(pos.all)) # for seeing how many NAs were removed
rownames(numteams.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")
colnames(numteams.pairwise.corr.matrix) <- c("QB1", "WR1", "WR2","WR3","RB1","RB2","RB3","TE1","TE2","DEF")

options(digits=2)

for (i in 1:ncol(pos.all)) {
  for (j in 1:ncol(pos.all)) {
    if (i <= j) {
    pos1 <- pos.all[,i]
    pos2 <- pos.all[,j]
    corr.vec <- rep(0,num.teams)
    for (k in 1:num.teams) {
      corr.vec[k] <- as.numeric(data[pos1[k], pos2[k]-BREAK[k]+1])
    }
    corr.vec <- corr.vec[!is.na(corr.vec)]
    numteams.pairwise.corr.matrix[i,j] <- length(corr.vec) # for seeing how many NAs were removed
    avg.pairwise.corr.matrix[i,j] <- mean(corr.vec)
    median.pairwise.corr.matrix[i,j] <- median(corr.vec)
    max.pairwise.corr.matrix[i,j] <- max(corr.vec)
    min.pairwise.corr.matrix[i,j] <- min(corr.vec)
    }
  }
}
View(avg.pairwise.corr.matrix)
View(median.pairwise.corr.matrix)
View(numteams.pairwise.corr.matrix)
View(max.pairwise.corr.matrix)
View(min.pairwise.corr.matrix)


#--------- Checking/debugging some individual pairs ---------#
# QB1, WR1
pos1 <- QB1
pos2 <- WR1
corr.vec <- rep(0,num.teams)
for (i in 1:num.teams) {
  corr.vec[i] <- as.numeric(data[pos1[i], pos2[i]-BREAK[i]+1])
}
print(paste0("Mean: ", mean(corr.vec)))
print(paste0("Median: ", median(corr.vec)))

# QB1, RB1
pos1 <- QB1
pos2 <- RB1
corr.vec <- rep(0,num.teams)
for (i in 1:num.teams) {
  corr.vec[i] <- as.numeric(data[pos1[i], pos2[i]-BREAK[i]+1])
}
corr.vec # NA in 28th element
QB1[28] # ah, QB1 and RB1 (Blaine Gabbert and Carlos Hyde) never played a game together, so corr is NA
corr.vec <- corr.vec[!is.na(corr.vec)] # remove the NA elements
length(corr.vec)

print(paste0("Mean: ", mean(corr.vec)))
print(paste0("Median: ", median(corr.vec)))

