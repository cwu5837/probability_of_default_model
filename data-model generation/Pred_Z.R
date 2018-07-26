setwd("H:/trans mtx showcase")

# load data
load("data/00 loan.RData")
load("data/00 macro.RData")
load("data/Z Score.RData")
load("data/final_model.RData")


source("functions.R")

Z_model_2f<-pred_z(Model_2f[1,],Z,macro)
Z_model_3f<-pred_z(Model_3f[1,],Z,macro)


save(Z_model_2f,Z_model_3f,file = "data/pred_Z.RData")
