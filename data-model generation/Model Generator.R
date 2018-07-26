setwd("H:/trans mtx showcase")

# load data
load("data/00 macro.RData")
load("data/Z Score.RData")

source("functions.R")

# load required packages
library(parallel)
library(doParallel)

# set up cores
cl = makeCluster(detectCores())  # Detects the number of cores
registerDoParallel(cl)           # Registers the number of cores
getDoParWorkers()

# generate models
Model_3 = model.select(Z$zlist$Zscore, macro$x, 3, 0.1)
Model_2 = model.select(Z$zlist$Zscore, macro$x, 2, 0.1)

# sign check
Model_3f= Sign.Check3(Model_3)
Model_2f = Sign.Check2(Model_2)

save(Model_3f,Model_2f, file = "data/final_model.RData")
