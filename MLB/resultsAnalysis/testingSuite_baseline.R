if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

######### Instructions #########

# After running, view Results using the following commands: 
# View(PnL[,c(1,2,6,7)])
# View(PnL$Lineups[[NUMBER]])
# View(aggregated_PnL)


#########   Variables   #########

# the Row number that corresponds to the desired contest in 'contest_info' 
#lineup_name <- "formulations.formulation8_covar_stacksize_5_overlap_5_lineups_150_lambda_0.005_exposure_P0.8_exposure_B10.5_exposure_B20.4_exposure_B30.6_exposure_C0.5_exposure_SS0.5_exposure_OF0.6_min_pitcher_exposure0.6_covariance_mat_update*"
#lineup_name <- "*formulation8*"
lineup_name <- "formulations.formulation5_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.3_exposure_SS0.5_exposure_OF0.6_covar_chg75p_exp(spike).csv"

######### Code Begins #########
library(data.table)
library(dplyr)
library(ggplot2)

### Save DFS Directory Path

original_wd <- getwd()

# Load in Helper Files
setwd('MLB/resultsAnalysis/helperFunctions')
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)

# Return to base directory
setwd(original_wd)

### Read in Contest File (Not necessary but useful to look at to find correct row)
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)
contest_baseline <- read.csv('MLB/optimizationCode/baseline_contests_$8.00.csv', stringsAsFactors = F)
contest_row_index <- contest_baseline$contest_row_index

# for (n in seq(50,150,25)) {
  PnL <- data.frame(Name=character(),
                   PnL=numeric(), 
                   Lineups=list(), 
                   contest=character(),
                   Date=as.Date(character()),
                   stringsAsFactors=FALSE) 
  # print(n)
  for(contest_row in contest_row_index) {
    temp <- singleContest_manyLineups_PnL_comparison(contest_row, lineup_name, n)
    if (!is.null(temp)) {
      temp$contest <- contest_info$Contest_Name[contest_row]
      temp$Date <- as.Date(contest_info$Contest_Date)[contest_row]
      PnL <- rbind(PnL,temp)
      # print(paste0(as.Date(contest_info$Contest_Date)[contest_row], " completed")) 
    }
  }
  print(sum(PnL$PnL))
# }

aggregated_PnL <- aggregate(PnL$PnL, by= list(PnL$Name), sum)

####### GRAPHS CUMSUM #####

final_data <- as.data.table(PnL[,c(1,2,6,7)])

final_data$Date <- as.Date(final_data$Date, "%m/%d/%Y")

# use data table to aggregate on months 
# First lets add a field plot date with Year and Month YYYYMM 201401
final_data[, PlotDate := as.numeric(format(Date, "%Y%m%d"))] 

# key by this plot date
setkeyv(final_data, "PlotDate")

# second we aggregate with by , and label columns
plotdata <- final_data[, .(PnL  = cumsum(PnL)), by = list(PlotDate, Name)]
plotdata = plotdata[order(plotdata$PlotDate), ]
plotdata <- mutate(group_by(plotdata,Name), cumsum=cumsum(PnL))


#Subset on any substring you want
#plotdata <- plotdata[grepl('formulation5',plotdata$Name),]

ggplot(data=plotdata,
       aes(x=as.Date(as.character(PlotDate),'%Y%m%d'), y=cumsum, colour=Name)) +
  geom_line() + theme(legend.position="none")



# view a particular formulation
# form_name <- "formulations.formulation9_covar_stacksize_5_overlap_5_lineups_150_lambda_0.001_exposure_P0.8_exposure_B10.5_exposure_B20.4_exposure_B30.6_exposure_C0.5_exposure_SS0.5_exposure_OF0.6_covar_chg75p_exp(spike).csv"
# form_name <- "formulations.formulation8_covar_stacksize_5_overlap_5_lineups_150_lambda_0.005_exposure_P0.8_exposure_B10.5_exposure_B20.4_exposure_B30.6_exposure_C0.5_exposure_SS0.5_exposure_OF0.6_min_pitcher_exposure0.6_covar_chg75p_exp(spike).csv"
# form_name <- "formulations.formulation5_covar_stacksize_5_overlap_5_lineups_150_lambda_0.0_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.3_exposure_SS0.5_exposure_OF0.6_no_covar.csv"
# form_name <- "formulations.formulation5_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.3_exposure_SS0.5_exposure_OF0.6_covar_chg75p_exp(spike).csv"
# form_name <- "formulations.formulation3_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_0.6_covar_chg75p_exp(spike)"
inds_form <- which(PnL$Name==form_name)
plot(as.Date(PnL$Date[inds_form]), PnL$PnL[inds_form], type = "b")
as.Date(PnL$Date[inds_form])
PnL$PnL[inds_form]
sum(PnL$PnL[inds_form]) # sum(PnL$PnL[inds_form[-27]])
sd(PnL$PnL[inds_form])
# sum(PnL$PnL[inds_form]) / sd(PnL$PnL[inds_form])
mean(PnL$PnL[PnL$PnL<0])
sd(PnL$PnL[PnL$PnL<0])

# formulation pnl df
pnl.df <- as.data.frame(matrix(data = NA, nrow = length(inds_form), ncol = 2, dimnames = list(NULL, c("Date", "PnL"))))
pnl.df$Date <- as.Date(PnL$Date[inds_form])
pnl.df$PnL <- PnL$PnL[inds_form]
pnl.df$PnL <- as.numeric(as.character(pnl.df$PnL))
# pnl.df <- pnl.df[order(pnl.df$PnL, decreasing = T),]

# aggregated_PnL
for (i in 1:nrow(aggregated_PnL)) {
  temp <- PnL$PnL[which(PnL$Name==aggregated_PnL$Group.1[i])]
  temp[temp > 60000] <- 60000
  aggregated_PnL$Adj_PnL[i] <- sum(temp)
  aggregated_PnL$Num_25k[i] <- sum(PnL$PnL[which(PnL$Name==aggregated_PnL$Group.1[i])] > 25000)
  aggregated_PnL$Max_Win[i] <- max(PnL$PnL[which(PnL$Name==aggregated_PnL$Group.1[i])])
}

# write to file
# write.csv(pnl.df, file = paste0("MLB/resultsAnalysis/analyze_generated_lineups/", substr(form_name, 1, nchar(form_name)-4), "/daily_pnl.csv"), row.names = F)

# save workspace variables
# save(list = ls(all.names = TRUE), file = "MLB/resultsAnalysis/baseline_PNL_2017-08-01.RData", envir = .GlobalEnv)
# load("MLB/resultsAnalysis/baseline_PNL_2017-08-01.RData")
# load("MLB/resultsAnalysis/baseline_PNL_$8.00.RData")
