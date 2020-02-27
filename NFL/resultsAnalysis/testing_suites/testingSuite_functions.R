#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

source('resultsAnalysis/Rscripts/helperFunctions/compute_lineup_fpts.R')
source('resultsAnalysis/Rscripts/helperFunctions/load_player_performance.R')
source('resultsAnalysis/Rscripts/helperFunctions/load_payout_structure.R')
source('resultsAnalysis/Rscripts/helperFunctions/load_full_contest_results.R')



####### SET PARAMETER VALUES #########
week <- 4
predictions.source <- "_dfn" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
source_actual_fpts <- 'DFN' # 'FC' or 'DFN'
contest_entry_fee <- "$20"

formulation <- 14

overlap.lo <- 4 # overlap.lo and overlap.hi must be the same if exposure.range is not from 1 to 1
overlap.hi <- 4

exposure.range <- seq(from = 0.4, to = 0.4, by = 0) # must be from 1 to 1 if overlap.lo != overlap.hi
exposure.pos.bool <- T # if TRUE then exposure.range is ignored, if FALSE then position exposures (def, wr, rb, te, qb) ignored
exposure.def <- 0.25
exposure.wr <- 0.25
exposure.rb <- 0.75
exposure.te <- 0.75
exposure.qb <- 0.5
exposure.valuewr <- "_valuewrexp_0.15" # "_valuewrexp_0.15" or ""
freqInd <- "" # _FreqInd or ""

num.lineups <- "" # "" or "_numlineups_1000"


####### LOAD DATA USING HELPER FUNCTIONS #########


contest_results <- load_full_contest_results(week, contest_entry_fee, "main_slate", 2016)
player_performance <- load_player_performance(week, 2016, source_actual_fpts)
payout_structure <- load_payout_structure_results(week, contest_entry_fee, "main_slate", 2016)
