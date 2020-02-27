#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/resultsAnalysis")

week.latest <- 5
#week.num <- 5
contest.entry.fee <- "$20"
predictions.source <- "_dfn" # Either "_dfn" or ""
formulation <- 6
overlap.lo <- 1
overlap.hi <- 9
exposure <- 1

all <- matrix(data = NA, nrow=9, ncol = 0)
all <- cbind(all, 1:9)
for (i in 2:week.latest) {
  temp <- readRDS(paste0("../resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", i, predictions.source, "_formulation", formulation, "_exposure_", exposure, ".rds"))
  all <- cbind(all, temp[,2])
}
all <- cbind(all, rowSums(all[,-1]))
colnames(all) <- c('Overlap','PnL_Wk2','PnL_Wk3','PnL_Wk4','PnL_Wk5', 'Sum')
saveRDS(all, file = paste0("../resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_allweeks", predictions.source, "_formulation", formulation, "_exposure_", exposure, ".rds"))


# Compare PnL matrices
form2entry20 <- readRDS(file = "../resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_allweeks_dfn_formulation2_exposure_1.rds")
form4entry20 <- readRDS(file = "../resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_allweeks_dfn_formulation4_exposure_1.rds")
form5entry20 <- readRDS(file = "../resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_allweeks_dfn_formulation5_exposure_1.rds")

