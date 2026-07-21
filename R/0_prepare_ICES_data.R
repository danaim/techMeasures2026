library(FLCore)
library(FLa4a)
library(FLBRP)
rm(list = ls())
load('data/ICES_data/FLStocks.RData')
stock_names <- ls()

sapply(mget(stock_names),class)

# Fix the cod.27.46a7d20; is a list

cod.27.46a7d20.N <- cod.27.46a7d20[[1]]
cod.27.46a7d20.S <- cod.27.46a7d20[[2]]
cod.27.46a7d20.V <- cod.27.46a7d20[[3]]

# Need to write back to ICES for the missing data

stks <- FLStocks(
  cod.27.12coastN = cod.27.12coastN,
  cod.27.46a7d20.N = cod.27.46a7d20.N, # missing F-at-age
  cod.27.46a7d20.S = cod.27.46a7d20.S, # missing F-at-age
  cod.27.46a7d20.V = cod.27.46a7d20.V, # missing F-at-age
  cod.27.7a       = cod.27.7a, 
  cod.27.7ek      = cod.27.7ek,
  had.27.46a20    = had.27.46a20, 
  had.27.6b       = had.27.6b, 
  had.27.7bk      = had.27.7bk,
  hke.27.3a468ab  = hke.27.3a468ab, 
  hke.27.8c9a     = hke.27.8c9a, 
  ldb.27.8c9      = ldb.27.8c9, # missing F-at-age
  meg.27.7bk8abd  = meg.27.7bk8abd, # missing F-at-age
  meg.27.8c9a     = meg.27.8c9a, # missing F-at-age
  ple.27.2132     = ple.27.2132, 
  ple.27.420      = ple.27.420,
  ple.27.7a       = ple.27.7a, 
  ple.27.7d       = ple.27.7d, 
  whg.27.47d      = whg.27.47d, 
  whg.27.6a       = whg.27.6a, 
  whg.27.7a       = whg.27.7a, # missing F-at-age
  whg.27.7bcek    = whg.27.7bcek
)

for(i in 1:length(stks)){
  name(stks[[i]]) <- names(stks)[i]
}

# For the moment we can check the available stock objects from the CFP
# stks[['cod.27.46a7d20.N']] <- readRDS("data/ICES_data/from_CFP2026/cod.27.46a7d20N.rds")
# stks[['cod.27.46a7d20.S']] <- readRDS("data/ICES_data/from_CFP2026/cod.27.46a7d20S.rds")
# stks[['cod.27.46a7d20.V']] <- readRDS("data/ICES_data/from_CFP2026/cod.27.46a7d20V.rds")
# stks[['ldb.27.8c9']] <- readRDS("data/ICES_data/from_CFP2026/ldb.27.8c9a.rds")
# stks[['meg.27.8c9a']] <- readRDS("data/ICES_data/from_CFP2026/meg.27.8c9a.rds")
# stks[['whg.27.7a']] <- readRDS("data/ICES_data/from_CFP2026/whg.27.7a.rds")

# Still missing the meg.27.7bk8abd and pok.27.3a46
# For now we save the FLStocks object
saveRDS(stks, file = "Robj/ICES_stks_prelim.rds")
