######### Instructions #########

# After running, view Results using the following commands: 
# View(pnl[1:2],)
# View(pnl$Lineups[[NUMBER]])



#########   Variables   #########

# the Row number that corresponds to the desired contest in 'contest_info' 
contest_row <- 2564

# Check Line 35 - 40 For the Regex Variable

######### Code Begins #########

# After running, view Results using the following commands: 
# View(pnl[1:2],)
# View(pnl$Lineups[[NUMBER]])

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

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

# Second Variable is a Regex that defaults to '*.csv' this does the calculation for all lineups 
    # - Some Suggested regex:
      # 'SPECIFIC LINEUP NAME' <- returns a dataframe with 1 row, only has that lineup 
      # '*_test.csv' Returns Lineup for the testing matrices
pnl <- singleContest_manyLineups_PnL_comparison(contest_row,
                                                "formulations.formulation5_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.3_exposure_SS0.5_exposure_OF0.6_covar_chg75p_exp(spike).csv",
                                                100)
pnl$PnL


