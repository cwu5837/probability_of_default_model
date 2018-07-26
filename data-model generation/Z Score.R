setwd("H:/trans mtx showcase")

# load data
load("data/00 loan.RData")

source("functions.R")

Z<-Z.Value(loan)

######## run t-test to check if mean is 0 #########
t.test(Z$zlist[,2])


library(EnvStats)
varTest(Z$zlist[,2])

save(Z,file = "data/Z Score.RData")