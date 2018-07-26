setwd("H:/trans mtx showcase")

# load data
load("data/00 loan.RData")
load("data/Z Score.RData")

source("functions.R")

df_bt = df.backtest(loan$Cnt, Z$zlist$Zscore, Z$rho, data.prep(loan)$cml_cnt)

df_act = df_rt(loan)

df_backtest = data.frame(df_act,df_bt)

colnames(df_backtest)=c("Year","Actual","Backtesting")


save(df_backtest, file = "data/df_backtest.RData")

