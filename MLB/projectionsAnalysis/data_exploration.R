if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Analyze projections. Using players from the $5 entry Knuckleball contest each day.
# - Correlations between each projection source and actual fpts
# - Regression (actual fpts vs projection sources)
# - Search for stacks
#
# TODO:
# - rank analysis (as opposed to absolute fpts)


####### Import Functions #######
source("MLB/functions_global/aggregateJuliaDF.R")


####### Create Aggregated Projected and Actual Fpts Dataframe For Each Day #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

# find one 150 entry contest per day
contest_info_filtered <- NULL
dates <- seq(from = as.Date("2017-04-02"), to = Sys.Date() - 1, by = "day")
for (i in 1:length(dates)) {
  # subset by the date and max_entry=150
  contest_info_temp <- contest_info[contest_info$Contest_Date==dates[i] & contest_info$Max_Entry==150, ]
  
  # use knuckleball contest if exists, else use first 150 entry contest in df
  temp.found <- F
  for (j in 1:nrow(contest_info_temp)) {
    if (grepl("KNUCKLEBALL", contest_info_temp$Contest_Name[j])==T) {
      contest_info_filtered <- rbind(contest_info_filtered, contest_info_temp[j,])
      temp.found <- T
    }
  }
  if (temp.found==F) {
    contest_info_filtered <- rbind(contest_info_filtered, contest_info_temp[1,])
  }
}
remove(contest_info_temp, temp.found)

# aggregate projectioned and actual fpts for each day's contest
aggregated_data_hitters <- list()
aggregated_data_pitchers <- list()
for (i in 1:nrow(contest_info_filtered)) {
  projections.dat <- aggregateJuliaDF(contest_info_filtered$Contest_Date[i], paste0(contest_info_filtered$Entry_Fee[i],"entry_",gsub(" ", "", contest_info_filtered$Contest_Name[i])))
  aggregated_data_hitters[[i]] <- projections.dat[[1]]
  aggregated_data_pitchers[[i]] <- projections.dat[[2]]
}
remove(projections.dat)


####### Filter by Projection > 0 and NAs (for both projection source and actual) ####### 
# hitters
rotogrinders_hitters_alldays <- NULL
dfn_hitters_alldays <- NULL
baseballmonster_hitters_alldays <- NULL
rotowire_hitters_alldays <- NULL
for (i in 1:nrow(contest_info_filtered)) {
  rotogrinders_hitters_alldays <- rbind(rotogrinders_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection > 0 & !is.na(aggregated_data_hitters[[i]]$Projection) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection", "Actual_fpts")])
  dfn_hitters_alldays <- rbind(dfn_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection_dfn > 0 & !is.na(aggregated_data_hitters[[i]]$Projection_dfn) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection_dfn", "Actual_fpts")])
  baseballmonster_hitters_alldays <- rbind(baseballmonster_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection_baseballmonster > 0 & !is.na(aggregated_data_hitters[[i]]$Projection_baseballmonster) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection_baseballmonster", "Actual_fpts")])
  rotowire_hitters_alldays <- rbind(rotowire_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection_rotowire > 0 & !is.na(aggregated_data_hitters[[i]]$Projection_rotowire) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection_rotowire", "Actual_fpts")])
}

# pitchers
rotogrinders_pitchers_alldays <- NULL
dfn_pitchers_alldays <- NULL
baseballmonster_pitchers_alldays <- NULL
rotowire_pitchers_alldays <- NULL
for (i in 1:nrow(contest_info_filtered)) {
  rotogrinders_pitchers_alldays <- rbind(rotogrinders_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection", "Actual_fpts")])
  dfn_pitchers_alldays <- rbind(dfn_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection_dfn > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection_dfn) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection_dfn", "Actual_fpts")])
  baseballmonster_pitchers_alldays <- rbind(baseballmonster_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection_baseballmonster > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection_baseballmonster) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection_baseballmonster", "Actual_fpts")])
  rotowire_pitchers_alldays <- rbind(rotowire_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection_rotowire > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection_rotowire) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection_rotowire", "Actual_fpts")])
}

####### View Quantiles #######
# note that these are different bc there are a different number of NAs for each projection source (filtered out in last step)
quantile(rotogrinders_hitters_alldays$Actual_fpts)
quantile(dfn_hitters_alldays$Actual_fpts)
quantile(baseballmonster_hitters_alldays$Actual_fpts)
quantile(rotowire_hitters_alldays$Actual_fpts)

quantile(rotogrinders_pitchers_alldays$Actual_fpts)
quantile(dfn_pitchers_alldays$Actual_fpts)
quantile(baseballmonster_pitchers_alldays$Actual_fpts)
quantile(rotowire_pitchers_alldays$Actual_fpts)


####### Check Correlations of Projections and Actual #######
# correlations for all players
print(paste0("Corr(Rotogrinders, Actual) [hitters all]: ", cor(rotogrinders_hitters_alldays$Projection, rotogrinders_hitters_alldays$Actual_fpts)))
print(paste0("Corr(DFN, Actual) [hitters all]: ", cor(dfn_hitters_alldays$Projection_dfn, dfn_hitters_alldays$Actual_fpts)))
print(paste0("Corr(BaseballMonster, Actual) [hitters all]: ", cor(baseballmonster_hitters_alldays$Projection_baseballmonster, baseballmonster_hitters_alldays$Actual_fpts)))
print(paste0("Corr(Rotowire, Actual) [hitters all]: ", cor(rotowire_hitters_alldays$Projection_rotowire, rotowire_hitters_alldays$Actual_fpts)))

print(paste0("Corr(Rotogrinders, Actual) [pitchers all]: ", cor(rotogrinders_pitchers_alldays$Projection, rotogrinders_pitchers_alldays$Actual_fpts)))
print(paste0("Corr(DFN, Actual) [pitchers all]: ", cor(dfn_pitchers_alldays$Projection_dfn, dfn_pitchers_alldays$Actual_fpts)))
print(paste0("Corr(BaseballMonster, Actual) [pitchers all]: ", cor(baseballmonster_pitchers_alldays$Projection_baseballmonster, baseballmonster_pitchers_alldays$Actual_fpts)))
print(paste0("Corr(Rotowire, Actual) [pitchers all]: ", cor(rotowire_pitchers_alldays$Projection_rotowire, rotowire_pitchers_alldays$Actual_fpts)))

# correlations for all players with projection > X (after filtering out projection > 0 and !is.na())
X <- 7.5
print(paste0("Corr(Rotogrinders, Actual) [hitters projection > ",X,"]: ", cor(rotogrinders_hitters_alldays$Projection[rotogrinders_hitters_alldays$Projection > X], rotogrinders_hitters_alldays$Actual_fpts[rotogrinders_hitters_alldays$Projection > X])))
print(paste0("Corr(DFN, Actual) [hitters projection > ",X,"]: ", cor(dfn_hitters_alldays$Projection_dfn[dfn_hitters_alldays$Projection_dfn > X], dfn_hitters_alldays$Actual_fpts[dfn_hitters_alldays$Projection_dfn > X])))
print(paste0("Corr(BaseballMonster, Actual) [hitters projection > ",X,"]: ", cor(baseballmonster_hitters_alldays$Projection_baseballmonster[baseballmonster_hitters_alldays$Projection_baseballmonster > X], baseballmonster_hitters_alldays$Actual_fpts[baseballmonster_hitters_alldays$Projection_baseballmonster > X])))
print(paste0("Corr(Rotowire, Actual) [hitters projection > ",X,"]: ", cor(rotowire_hitters_alldays$Projection_rotowire[rotowire_hitters_alldays$Projection_rotowire > X], rotowire_hitters_alldays$Actual_fpts[rotowire_hitters_alldays$Projection_rotowire > X])))

X <- 15
print(paste0("Corr(Rotogrinders, Actual) [pitchers projection > ",X,"]: ", cor(rotogrinders_pitchers_alldays$Projection[rotogrinders_pitchers_alldays$Projection > X], rotogrinders_pitchers_alldays$Actual_fpts[rotogrinders_pitchers_alldays$Projection > X])))
print(paste0("Corr(DFN, Actual) [pitchers projection > ",X,"]: ", cor(dfn_pitchers_alldays$Projection_dfn[dfn_pitchers_alldays$Projection_dfn > X], dfn_pitchers_alldays$Actual_fpts[dfn_pitchers_alldays$Projection_dfn > X])))
print(paste0("Corr(BaseballMonster, Actual) [pitchers projection > ",X,"]: ", cor(baseballmonster_pitchers_alldays$Projection_baseballmonster[baseballmonster_pitchers_alldays$Projection_baseballmonster > X], baseballmonster_pitchers_alldays$Actual_fpts[baseballmonster_pitchers_alldays$Projection_baseballmonster > X])))
print(paste0("Corr(Rotowire, Actual) [pitchers projection > ",X,"]: ", cor(rotowire_pitchers_alldays$Projection_rotowire[rotowire_pitchers_alldays$Projection_rotowire > X], rotowire_pitchers_alldays$Actual_fpts[rotowire_pitchers_alldays$Projection_rotowire > X])))

# correlations for all players with Actual_fpts > X (after filtering out projection > 0 and !is.na())
X <- 7.5
print(paste0("Corr(Rotogrinders, Actual) [hitters Actual_fpts > ",X,"]: ", cor(rotogrinders_hitters_alldays$Projection[rotogrinders_hitters_alldays$Actual_fpts > X], rotogrinders_hitters_alldays$Actual_fpts[rotogrinders_hitters_alldays$Actual_fpts > X])))
print(paste0("Corr(DFN, Actual) [hitters Actual_fpts > ",X,"]: ", cor(dfn_hitters_alldays$Projection_dfn[dfn_hitters_alldays$Actual_fpts > X], dfn_hitters_alldays$Actual_fpts[dfn_hitters_alldays$Actual_fpts > X])))
print(paste0("Corr(BaseballMonster, Actual) [hitters Actual_fpts > ",X,"]: ", cor(baseballmonster_hitters_alldays$Projection_baseballmonster[baseballmonster_hitters_alldays$Actual_fpts > X], baseballmonster_hitters_alldays$Actual_fpts[baseballmonster_hitters_alldays$Actual_fpts > X])))
print(paste0("Corr(Rotowire, Actual) [hitters Actual_fpts > ",X,"]: ", cor(rotowire_hitters_alldays$Projection_rotowire[rotowire_hitters_alldays$Actual_fpts > X], rotowire_hitters_alldays$Actual_fpts[rotowire_hitters_alldays$Actual_fpts > X])))

X <- 15
print(paste0("Corr(Rotogrinders, Actual) [pitchers Actual_fpts > ",X,"]: ", cor(rotogrinders_pitchers_alldays$Projection[rotogrinders_pitchers_alldays$Actual_fpts > X], rotogrinders_pitchers_alldays$Actual_fpts[rotogrinders_pitchers_alldays$Actual_fpts > X])))
print(paste0("Corr(DFN, Actual) [pitchers Actual_fpts > ",X,"]: ", cor(dfn_pitchers_alldays$Projection_dfn[dfn_pitchers_alldays$Actual_fpts > X], dfn_pitchers_alldays$Actual_fpts[dfn_pitchers_alldays$Actual_fpts > X])))
print(paste0("Corr(BaseballMonster, Actual) [pitchers Actual_fpts > ",X,"]: ", cor(baseballmonster_pitchers_alldays$Projection_baseballmonster[baseballmonster_pitchers_alldays$Actual_fpts > X], baseballmonster_pitchers_alldays$Actual_fpts[baseballmonster_pitchers_alldays$Actual_fpts > X])))
print(paste0("Corr(Rotowire, Actual) [pitchers Actual_fpts > ",X,"]: ", cor(rotowire_pitchers_alldays$Projection_rotowire[rotowire_pitchers_alldays$Actual_fpts > X], rotowire_pitchers_alldays$Actual_fpts[rotowire_pitchers_alldays$Actual_fpts > X])))

# Takeaways:
# - rotogrinders seems to be better at predicting pitchers that do relatively well (X = 7.5, 15)
# - rotowire seems to be better at predicting hitters based on projection > X (but not observed for Actual_fpts > X)


####### Regression #######
# aggregate hitters and pitchers from all days
all_days_hitters <- NULL
all_days_pitchers <- NULL
for (i in 1:length(aggregated_data_hitters)) {
  all_days_hitters <- rbind(all_days_hitters, aggregated_data_hitters[[i]])
  all_days_pitchers <- rbind(all_days_pitchers, aggregated_data_pitchers[[i]])
}

# omit rows with NAs
all_days_hitters <- na.omit(all_days_hitters)
all_days_pitchers <- na.omit(all_days_pitchers)

# OLS for rows w/o NAs
model_hitters_ols <- lm(Actual_fpts ~ Projection + Projection_dfn + Projection_baseballmonster + Projection_rotowire, data = all_days_hitters)
summary(model_hitters_ols)
plot(model_hitters_ols$residuals)

model_pitchers_ols <- lm(Actual_fpts ~ Projection + Projection_dfn + Projection_baseballmonster + Projection_rotowire, data = all_days_pitchers)
summary(model_pitchers_ols)
plot(model_pitchers_ols$residuals)

# filter by Actual_fpts > X
X <- quantile(all_days_hitters$Actual_fpts)['75%']
all_days_hitters <- all_days_hitters[all_days_hitters$Actual_fpts > X,]
X <- quantile(all_days_pitchers$Actual_fpts)['75%']
all_days_pitchers <- all_days_pitchers[all_days_pitchers$Actual_fpts > X,]

# OLS for rows w/o NAs and Actual_fpts > X
model_hitters_ols <- lm(Actual_fpts ~ Projection + Projection_dfn + Projection_baseballmonster + Projection_rotowire, data = all_days_hitters)
summary(model_hitters_ols)
plot(model_hitters_ols$residuals)

model_pitchers_ols <- lm(Actual_fpts ~ Projection + Projection_dfn + Projection_baseballmonster + Projection_rotowire, data = all_days_pitchers)
summary(model_pitchers_ols)
plot(model_pitchers_ols$residuals)

# Takeaways:
# - Coefficient for DFN is significant for predicting hitters (all hitters as well as Actual_fpts > 75th perecentile)
# - BaseballMonster is particularly bad for predicting hitters: coefficient is ~0.0 for all hitters, and is negative for hitters s.t. Actual_fpts > 75th perecentile
# - Not enough data points for pitchers yet (2017-04-18)
# - R^2's tend to be very low (< 0.10 for hitters and < 0.05 for pitchers)


####### Search for Stacks #######







