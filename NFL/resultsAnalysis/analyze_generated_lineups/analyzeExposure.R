#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

week.latest <- 5
contest.entry.fee <- "$20"
predictions.source <- "_dfn" # Either "_dfn" or ""
formulation <- 5
overlap <- 4
exposure.range <- seq(from = 0.1, to = 1, by = 0.1)

all <- matrix(data = NA, nrow=10, ncol = 0)
all <- cbind(all, exposure.range)
for (i in 2:week.latest) {
  temp <- readRDS(paste0("resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", i, predictions.source, "_formulation", formulation, "_overlap_", overlap, ".rds"))
  all <- cbind(all, temp[,2])
}
all <- cbind(all, rowSums(all[,-1]))
colnames(all) <- c('Exposure','PnL_Wk2','PnL_Wk3','PnL_Wk4','PnL_Wk5', 'Sum')
saveRDS(all, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_allweeks", predictions.source, "_formulation", formulation, "_overlap_", overlap, ".rds"))


# Compare PnL matrices
#form2entry20 <- readRDS(file = "resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_allweeks_dfn_formulation2_exposure_1.rds")

