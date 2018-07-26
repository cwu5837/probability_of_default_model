setwd("H:/trans mtx showcase")

# load data
load("data/00 loan.RData")
load("data/00 macro.RData")
load("data/Z Score.RData")
load("data/final_model.RData")


source("functions.R")

DF_model_2f<-pred_df(Model_2f[1,],Z,macro)
DF_model_3f<-pred_df(Model_3f[1,],Z,macro)


save(DF_model_2f,DF_model_3f,file = "data/pred_DF.RData")
