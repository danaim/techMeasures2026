library(FLCore)
library(ggpubr)
rm(list = ls())
source("R/_plot_functions.R")
source("R/ratios_functions.R")

stks_ICES <- readRDS("Robj/ICES_stks_prelim.rds")
stks_MED <- readRDS("Robj/MED_stks.rds")

lapply(stks_ICES,plotGridSelInd)

stks[[15]]
wireframe(harvest(stks[[15]]))
mat(stks[[15]])
lapply(stks,name)
plot(harvest(stks[[15]]))
