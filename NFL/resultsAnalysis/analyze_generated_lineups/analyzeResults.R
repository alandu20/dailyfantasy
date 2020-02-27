#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/resultsAnalysis")
#setwd("~/Projects/DFS/resultsAnalysis")

data.week1 <- read.csv("data_warehouse/personal_lineups_results/results_week1.csv", stringsAsFactors = F)
data.week2 <- read.csv("data_warehouse/personal_lineups_results/results_week2.csv", stringsAsFactors = F)
data.week3 <- read.csv("data_warehouse/personal_lineups_results/results_week3.csv", stringsAsFactors = F)

max.points.week1 <- 250.52
min.winner.week1 <- 135
min.percentile.winner.week1 <- 0.63

max.points.week2 <- 238.12
min.winner.week2 <- 143
min.percentile.winner.week2 <- 0.73

max.points.week3 <- 248.2
min.winner.week3 <- 144.3
min.percentile.winner.week3 <- 1-135550/490196

data.week1$Percentile_Place <- 1-data.week1$Place/data.week1$Contest_Entries
data.week2$Percentile_Place <- 1-data.week2$Place/data.week2$Contest_Entries
data.week3$Percentile_Place <- 1-data.week3$Place/data.week3$Contest_Entries

par(mfrow=c(1,3))
hist(data.week1$Percentile_Place, ylim=c(0,35))
abline(v=min.percentile.winner.week1, col = 'red') # everything to the right of this line is ITM
hist(data.week2$Percentile_Place, ylim=c(0,35))
abline(v=min.percentile.winner.week2, col = 'red')
hist(data.week3$Percentile_Place, ylim=c(0,35))
abline(v=min.percentile.winner.week3, col = 'red')

##

data.week1$Percentile_Points <- data.week1$Points/max.points.week1
data.week2$Percentile_Points <- data.week2$Points/max.points.week2
data.week3$Percentile_Points <- data.week3$Points/max.points.week3

par(mfrow=c(1,3))
hist(data.week1$Percentile_Points, ylim=c(0,35), xlim=c(0,1))
abline(v=min.winner.week1/max.points.week1, col = 'red')
hist(data.week2$Percentile_Points, ylim=c(0,35), xlim=c(0,1))
abline(v=min.winner.week2/max.points.week2, col = 'red')
hist(data.week3$Percentile_Points, ylim=c(0,35), xlim=c(0,1))
abline(v=min.winner.week2/max.points.week3, col = 'red')

plot(data.week1$Percentile_Place, data.week1$Percentile_Points)
plot(data.week2$Percentile_Place, data.week2$Percentile_Points)
plot(data.week3$Percentile_Place, data.week3$Percentile_Points)

