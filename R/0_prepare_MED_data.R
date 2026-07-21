library(FLCore)
rm(list = ls())

stks <- FLStocks(
  hke_1_5_6_7 = readRDS("data/stock_objects_MED/HKE_1_5_6_7.rds"),
  hke_8_9_10_11 = readRDS("data/stock_objects_MED/HKE_8_9_10_11.rds"),
  hke_12_13_14_15_16 = readRDS("data/stock_objects_MED/HKE_12_13_14_15_16.rds"),
  hke_17_18 = readRDS("data/stock_objects_MED/HKE_17_18.rds"),
  hke_19 = readRDS("data/stock_objects_MED/HKE_19.rds"),
  hke_20 = readRDS("data/stock_objects_MED/HKE_20.rds"),
  hke_22 = readRDS("data/stock_objects_MED/HKE_22.rds"),
  mut_1 = readRDS("data/stock_objects_MED/MUT_1.rds"),
  mut_6 = readRDS("data/stock_objects_MED/MUT_6.rds"),
  mut_9 = readRDS("data/stock_objects_MED/MUT_9.rds"),
  mut_19 = readRDS("data/stock_objects_MED/MUT_19.rds"),
  mut_20 = readRDS("data/stock_objects_MED/MUT_20.rds"), 
  mut_22 = readRDS("data/stock_objects_MED/MUT_22.rds")
)

saveRDS(stks, file = "Robj/MED_stks.rds")

df <- data.frame(Region = "Mediterranean Sea", 
           Species = c(rep('Hake',7),rep('Mullus barbatus',6)),
           Stock = names(stks),
          `Working Group` = c(rep("STECF",2), rep("GFCM",3),
                              rep("STECF",5), rep("GFCM",3)))
