library(dplyr)
library(reshape2)
library(stringr)


##### transform raw loan data into a list of transition matrices
matrix.list = function(matrix,type)
{
  mtx_list = list()
  # counting the number of matrices
  mtx_length = length(unique(matrix[,1]))
  
  if (type=="p") {
    for (i in 1:mtx_length)
    {
      start=(i*nrow(matrix)/mtx_length)-nrow(matrix)/mtx_length+1
      end=i*nrow(matrix)/mtx_length
      mtx_list[[i]] = matrix[start:end,4:11]
    }
  }
  else {
    for (i in 1:mtx_length)
    {
      start=(i*nrow(matrix)/mtx_length)-nrow(matrix)/mtx_length+1
      end=i*nrow(matrix)/mtx_length
      mtx_list[[i]] = matrix[start:end,12:19]
    }
  }
  
  
  return(mtx_list)
}


##### calculate R*
delta.transition = function(rho, Zt,cml)
{
  bins_mtx = qnorm(1 - cml, mean = 0, sd = 1)
  delta = pnorm((bins_mtx[, -ncol(bins_mtx)] - sqrt(rho)*Zt)/sqrt(1 - rho), mean = 0, sd = 1, log = FALSE) - 
    pnorm((bins_mtx[, -1] - sqrt(rho)*Zt)/sqrt(1 - rho), mean = 0, sd = 1, log = FALSE)
  return(delta)
}


##### Improve the accuracy of rho digit by digit
rho.iteration = function(max_itr, prop_list, bal_list, cml, avgpd)
{
  
  optimize.problem = function(rho, Zt, observed, startingcount)
  {
    startingcount2 = matrix(rowSums(startingcount), byrow = FALSE)
    stressed = delta.transition(rho, Zt,cml)
    delta = stressed - observed
    min_op = sum(startingcount2[1:nrow(startingcount2),1]*avgpd*delta)^2
    return(min_op)
  }
  
  find.z = function(lower, upper, n_itr)
  {
    var_list = c()
    mean_list = c()
    
    for (rho in seq(lower, upper, 1/10^n_itr))
    {
      z_list = c()
      min_obj = c()
      
      for (i in 1:length(prop_list))
      {
        OP = optimize(f             = optimize.problem,
                      rho           = rho,
                      observed      = prop_list[[i]],
                      startingcount = bal_list[[i]],
                      interval      = c(-5, 5),
                      tol           = .Machine$double.eps^10)
        z_list[i] = OP$minimum
        min_obj[i] = OP$objective
      }
      
      var_list[length(var_list)+1] = var(z_list)
      mean_list[length(mean_list)+1] = mean(z_list)
    }
    
    index_1 = which.min(abs(var_list - 1))
    index_2 = ifelse(var_list[index_1] < 1, index_1 - 1, index_1 + 1)
    rho_left = lower + (ifelse(index_1 > index_2, index_2, index_1) - 1)*1/10^n_itr
    rho_right = lower + (ifelse(index_1 > index_2, index_1, index_2) - 1)*1/10^n_itr
    
    bound = list(lower = rho_left, upper = rho_right, zlist = z_list)
    return(bound)
  }
  
  lower = 0
  upper = 0.99
  
  for(n_itr in 1:max_itr)
  {
    itr = find.z(lower, upper, n_itr)
    lower = itr$lower
    upper = itr$upper
  }
  Period = seq(1998, 2016, 1)
  zlist = data.frame(Period, itr$zlist)
  colnames(zlist) = c("Period", "Zscore")
  result = list("rho" = round(lower, max_itr-1),"zlist" = zlist)
  
  return(result)
}

##### calculate cumulative prob. 
data.prep = function(loan)
{
  count_list = matrix.list(loan,"c")

  count_prop_list = matrix.list(loan,"p")
  
  t = cbind(loan$Rating, select(loan,contains("_cnt")))
  colnames(t) = c("Rating","Aaa","Aa","A","Baa","Ba","B","Caa.C","Default")
  t$Rating = ordered(t$Rating, levels = c("Aaa","Aa","A","Baa","Ba","B","Caa.C"))
  count_avg = acast(melt(t, id = "Rating"), Rating ~ variable, fun.aggregate = sum, value.var = "value")
  count_avg_prop = prop.table(count_avg, 1)
  
  # Cummulate Probability
  cml_cnt = cbind(rep(0, nrow(count_avg_prop)), t(apply(count_avg_prop,1,cumsum)))
  cml_cnt[,ncol(count_avg_prop)+1] = 1
  cml_cnt[which(cml_cnt>1)] = 1
  
  # Constrain # 1.
  avgpd_cnt = matrix(c(count_avg_prop[,ncol(count_avg_prop)], 1.0), 
                     nrow(count_avg_prop), ncol(count_avg_prop), 
                     byrow = TRUE)
  
  return(data = list("count_prop_list" = count_prop_list, 
                     "count_list" = count_list,
                     "count_avg" = count_avg,
                     "count_avg_prop" = count_avg_prop,
                     "cml_cnt" = cml_cnt, 
                     "avgpd_cnt" = avgpd_cnt))
}

##### Finding z and the optimal rho
Z.Value = function(dataInput)
{
  data = data.prep(dataInput)
  
  Count.iter = rho.iteration(6, data$count_prop_list, data$count_list, data$cml_cnt, data$avgpd_cnt)
  return(Count.iter)
}


##### calculate the actual default rate
df_rt = function(dataInput) {
  data=dataInput%>%
    group_by(Year)%>%
    summarise(df.rate=sum(Default_cnt)/sum(Cnt))
  return(data)
}

##### backtest default rate using Z scores
df.backtest = function(cnt, zlist, rho, cml)
{
  starting = matrix(cnt, byrow = FALSE, nrow = 7)
  
  df_rt = c()
  
  for (i in 1:(length(zlist)))
  {
    t = delta.transition(rho, zlist[i], cml)
    ending = starting[,i] %*% t
    df_rt[i] = dplyr::last(ending)/sum(ending)
  }
  
  return(df_rt)
}


##### model selection
model.select=function(y,x,n,tol){
  ##First, we need to make sure that no duplicated varaibles selected for the same model
  var=combn(colnames(x[,-1]),n) 
  var=t(var)
  row=nrow(var)
  var.prep=var
  ##extract the original variable names
  variable.pattern="[A-Z]+[1-9]?[A-Z]?[a-z]?"
  index=NULL
  for (i in 1:row){
    for (j in 1:n){
      var.prep[i,j]=str_extract(var.prep[i,j],variable.pattern)
    }
    if (length(unique(var.prep[i,]))==n){
      ##index meaning rows with no duplicated variables
      index=c(index,i)
    }
  }
  variable.data=var[index,]
  w = foreach(k = 1:nrow(variable.data),.combine='rbind',.packages = c('lmtest','car','tseries')) %dopar%   
  {##Create linear model==================================
    model.data=cbind(x[variable.data[k,]])
    model=lm(y~.,data=model.data)
    summary=summary(model)
    ##Extract the coefficients and signs
    coef=as.data.frame(summary$coefficients)
    coef$sign="+"
    coef$sign[coef$Estimate<=0]="-"
    
    # Stationarity test for residuals (Augmented Dickey-Fuller)
    adf_pval = adf.test(summary$residuals)$p.val
    pp_pval = PP.test(summary$residuals)$p.val
    
    # Normality test for residuals
    sw_pval = shapiro.test(summary$residuals)$p.val

    # Hetereoscedasticity Test
    bp_pval = bptest(model)$p.val
    
    # Multicollinearity test (Variance Inflation Factor)
    vif_x = vif(model)
    
    # Autocorrelation test (Durbin-Watson)
    dw_pval = dwtest(model,alternative="two.sided")$p.val
    
    result.combined=c(variable.data[k,],
                      summary$coefficients[,"Estimate"],
                      coef$sign,
                      summary$coefficients[,"Pr(>|t|)"],
                      summary$r.squared,
                      summary$adj.r.squared,
                      adf_pval,
                      pp_pval,
                      vif_x,
                      dw_pval,
                      sw_pval,
                      bp_pval
    )
    return(result.combined)
  }
  ##name the columns
  model.summary = data.frame(w)
  base = c("Intercept",paste("var",1:n,sep=""))
  var_name = paste(base[-1],"name",sep="_")
  coef_name = paste(base,"coef",sep="_")
  sign_name = paste(base,"sign",sep="_")
  pval_name = paste(base,"pval",sep="_")
  vif_name = paste(base[-1],"vif",sep="_")
  dw_name = "DW_pval"
  adf_name = "ADF_pval"
  pp_name = "PP_pval"
  sw_name = "SW_pval"
  bp_name = "BP_pval"
  r2_name = "Rsq"
  adjr2_name = "adj.Rsq" 
  final_name = c(var_name,coef_name,sign_name,pval_name,r2_name,adjr2_name,adf_name,pp_name,vif_name,dw_name
                 ,sw_name,bp_name
  )
  colnames(model.summary) = final_name
  
  ###Filtering =======================================================
  filtered.columns=c(pval_name,adf_name,pp_name,vif_name,dw_name
                     ,sw_name,bp_name
  )[-1]
  data<- lapply(model.summary[,filtered.columns], function(x) as.numeric(as.character(x)))
  data=as.data.frame(data)
  col.smaller=c(pval_name)[-1]
  ##we need p val of variable smaller than tolerance + adf smaller than it
  VIF_max = apply(dplyr::select(data,contains("vif")),1,max)
  #keep=c(rowSums(data[,col.smaller]<=tol)==(n+1) & data["DW_pval"]>=tol & data["SW_pval"] & data["BP_pval"] & VIF_max<=10)
  keep=c(rowSums(data[,col.smaller]<=tol)==(n) & (data["PP_pval"]<=tol|data["ADF_pval"]<=tol) 
         & data["DW_pval"]>=tol
         & data["SW_pval"]>=tol
         & data["BP_pval"]>=tol
         & VIF_max<=10)
  model.filtered=model.summary[keep,]%>% arrange(desc(as.numeric(as.character(adj.Rsq))))
  return(model.filtered)
}


##### checking signs for coefficients for 3 factor model
Sign.Check3 = function(Model.Data)
{
  Model.Data = as.data.frame(Model.Data)
  
  Model.Data$SignCheck1=paste(Model.Data$var1_name,Model.Data$var1_sign)
  Model.Data$SignCheck2=paste(Model.Data$var2_name,Model.Data$var2_sign)
  Model.Data$SignCheck3=paste(Model.Data$var3_name,Model.Data$var3_sign)
  
  Model.Data$SignCheck1=gsub("_Ygr","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ygr","",Model.Data$SignCheck2)
  Model.Data$SignCheck3=gsub("_Ygr","",Model.Data$SignCheck3)
  
  Model.Data$SignCheck1=gsub("_Ydf","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ydf","",Model.Data$SignCheck2)
  Model.Data$SignCheck3=gsub("_Ydf","",Model.Data$SignCheck3)
  
  ####Second, remove the lag name
  Model.Data$SignCheck1=gsub(".*_","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub(".*_","",Model.Data$SignCheck2)
  Model.Data$SignCheck3=gsub(".*_","",Model.Data$SignCheck3)
  
  WrongSign = cbind("Unemp.Rt +",
                      "RGDP.Ygr -",
                      "NGDP.Ygr -",
                      "CPI -",
                      "DJIA -",
                      "BBB.Spd +",
                      "VIX +",
                      "NDI.Ygr -",
                      "RDI.Ygr -")
  
  filter.Data = Model.Data %>% filter(!(SignCheck1 %in% WrongSign)) %>% 
    filter(!(SignCheck2 %in% WrongSign))  %>% 
    filter(!(SignCheck3 %in% WrongSign)) %>% 
    select(-26,-27,-28)
  
  return(filter.Data)
}

##### checking signs for coefficients for 2 factor model
Sign.Check2 = function(Model.Data)
{
  Model.Data = as.data.frame(Model.Data)
  
  Model.Data$SignCheck1=paste(Model.Data$var1_name,Model.Data$var1_sign)
  Model.Data$SignCheck2=paste(Model.Data$var2_name,Model.Data$var2_sign)
  
  Model.Data$SignCheck1=gsub("_Ygr","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ygr","",Model.Data$SignCheck2)
  
  Model.Data$SignCheck1=gsub("_Ydf","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ydf","",Model.Data$SignCheck2)
  
  ####Second, remove the lag name
  Model.Data$SignCheck1=gsub(".*_","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub(".*_","",Model.Data$SignCheck2)
  
  WrongSign = cbind("Unemp.Rt +",
                    "RGDP.Ygr -",
                    "NGDP.Ygr -",
                    "CPI -",
                    "DJIA -",
                    "BBB.Spd +",
                    "VIX +",
                    "NDI.Ygr -",
                    "RDI.Ygr -")
  
  filter.Data = Model.Data %>% filter(!(SignCheck1 %in% WrongSign)) %>% 
    filter(!(SignCheck2 %in% WrongSign))  %>% 
    select(-26,-27)
  
  return(filter.Data)
}


##### predict z score
pred_z<-function(model,Z,macro){
  
  variable = model %>% select(contains("name")) %>% apply(2, FUN = as.character)
  coef = model %>% select(contains("coef")) %>% apply(2, FUN = as.numeric)
  
  Actual = Z$zlist$Zscore
  Fitted = as.matrix(cbind(1, macro$x[,variable])) %*% coef
  Base    = as.matrix(cbind(1, macro$b[,variable])) %*% coef
  Adverse = as.matrix(cbind(1, macro$a[,variable])) %*% coef
  Severe  = as.matrix(cbind(1, macro$s[,variable])) %*% coef
  
  data = data.frame(Year = c(rep(1998:2016, 2), rep(2016:2019, 3)),
                 scenario = c(rep("Actual", 19), rep("Fitted", 19), rep("Base", 4), rep("Adverse", 4), rep("Severe", 4)),
                 value = c(Actual, Fitted, Base, Adverse, Severe))
  
  data$scenario = ordered(as.factor(data$scenario), levels = c("Actual", "Fitted", "Severe", "Adverse", "Base"))
  
  return(data)
  
}

##### transform fitted Z to default rate
BackTest.Count.Default.Rate = function(X, rho,prez)
{
  c = NULL
  for (i in 1:19) {
    a=rowSums(X$count_list[[i]])
    c=cbind(c,a)
  }
  
  DF_rt = c()
  
  zscore = filter(prez,scenario=="Fitted")
  
  for (i in 1:19)
  {
    t = delta.transition(rho, prez$value[i], X$cml_cnt)
    startingcount = c[,i]%*%t
    df = dplyr::last(startingcount)/sum(startingcount)
    DF_rt[i] = df
  }
  
  return(DF_rt)
}

##### use predict trans mtx to calculate default rate
prdamt = function(transmtx, rateDim, X)
{
  starting = t(colSums(X$count_list[[19]][,-rateDim]))
  result = c()
  
  for(j in 1:3)
  {
    mxt=transmtx[,-rateDim,j]
    t = t(mxt)
    ending = starting %*% t
    dfrate = dplyr::last(ending)/sum(ending)
    result =rbind(result, dfrate)
    starting = ending[1:(length(ending) - 1)]
  }
  
  return(result)
}

##### forecast default rate for upcoming periods
dfprediction<-function(macro,Z,X,model) {
  # rho for counts
  rho = Z$rho
  
  selectModel <- model
  
  n <- nrow(selectModel)
  Beta <- select(selectModel,contains("coef"))
  Var <- select(selectModel,contains("name"))
  Sign <- select(selectModel,contains("sign"))
  Adj_r2 <- selectModel$adj.Rsq
  
  forecastY <- function (beta, macro, macroName)
  {
    indepVar <- cbind(1,macro[ ,macroName])
    Yforecast <- apply(indepVar, 1, function(x) sum(x*beta))
    return (Yforecast)
  }
  
  VarName = as.character(unlist(Var[1,]))
  Coef = as.numeric(as.character(unlist(Beta[1,])))
  
  # forecast Z score for diff scenarios
  Base <- forecastY(Coef,macro$b[-1,],VarName)
  Adverse <- forecastY(Coef,macro$a[-1,],VarName)
  Severe<- forecastY(Coef,macro$s[-1,],VarName)
  
  # cml
  X=data.prep(loan)
  avgqmatrix = X$count_avg_prop
  avgqmatrix0 <- rbind(avgqmatrix,c(rep(0,7),1))
  avgqmatrix1 <- t(apply(avgqmatrix0,1,rev))   
  avgqmatrix2 <- t(apply(avgqmatrix1,1,cumsum))
  cummulativeprob <- t(apply(avgqmatrix2,1,rev))
  avgMatrix <- cummulativeprob
  avgMatrix[,1] <- 1
  avgMatrix[avgMatrix>1] <- 1
  
  rateDim=8
  
  mertonModel <- function (Zscore, avgMatrix, rho)
  {
    quant <- apply(avgMatrix, 2, qnorm)
    quant2 <- (quant-sqrt(rho)*Zscore[1])/sqrt(1-rho)
    foreMatrix <- apply(quant2, 2, pnorm)
    foreMatrix1 <- foreMatrix-cbind(foreMatrix[,2:rateDim],0)
    foreMatrix1[rateDim,1:rateDim-1]=0  
    foreMatrix1[rateDim,rateDim]=1.0  
    return (foreMatrix1)
  }
  
  
  trans.matrix_gen <- function(Zscore)
  {        
    transMatrix <- NULL
    for (i in 1:3)
    {transMatrix <- rbind(transMatrix, mertonModel(Zscore[i],avgMatrix,rho))}
    transMatrix <- array(c(t(transMatrix)), dim = c(rateDim,rateDim,3))
    return(transMatrix)
  }
  
  B<-trans.matrix_gen(Base)
  A<-trans.matrix_gen(Adverse)
  S<-trans.matrix_gen(Severe)
  
  base<-prdamt(B, rateDim, X)
  adverse<-prdamt(A, rateDim, X)
  severe<-prdamt(S, rateDim, X)
  
  return(data.frame("Base_DF_Rt" = base, "Adverse_DF_Rt" = adverse,"Severe_DF_Rt" = severe))
}

##### predict default rate
pred_df<-function(model,Z,macro){
  prez=pred_z(model,Z,macro)
  rho=Z$rho
  X=data.prep(loan)
  # actual df rt
  actual=(df_rt(loan)$df.rate)*100
  # fitted df rt
  fit=(BackTest.Count.Default.Rate(X,rho,prez))*100
  # forecast df rt
  forecast_df=dfprediction(macro,Z,X,model)
  base=forecast_df$Base_DF_Rt*100
  adverse=forecast_df$Adverse_DF_Rt*100
  severe=forecast_df$Severe_DF_Rt*100
  
  data = data.frame(Year = c(rep(1998:2016, 2), rep(2016:2019, 3)),
                    scenario = c(rep("Actual", 19), rep("Fitted", 19), rep("Base", 4), rep("Adverse", 4), rep("Severe", 4)),
                    value = c(actual, fit, fit[19],base,fit[19],adverse,fit[19],severe))
  
  data$scenario = ordered(as.factor(data$scenario), levels = c("Actual", "Fitted", "Severe", "Adverse", "Base"))
  
  return(data)
  
  
}

##### print model formula
model_formula<-function(selectModel,num_vars) {
  Beta <- selectModel%>%
    select(contains("coef"))%>%
    unlist()%>%
    as.character()%>%
    as.numeric()%>%
    round(2)
  
  Var <- selectModel%>%
    select(contains("name"))%>%
    unlist()%>%
    as.character()
  Var <-gsubfn("_",".",Var)
  
  
  Adj_r2 <- selectModel$adj.Rsq%>%
    as.character()%>%
    as.numeric()%>%
    round(2)
  
  if (num_vars == 3) {
    formula=paste("$$", Beta[1], "+", Beta[2], "\\times", Var[1], "+", Beta[3], "\\times", Var[2], "+", Beta[4], "\\times", Var[3], "$$")
  } else {
    formula=paste("$$", Beta[1], "+", Beta[2], "\\times", Var[1], "+", Beta[3], "\\times", Var[2], "$$")
  }
  
  adjr = paste("$$ Adjusted R^2:", Adj_r2, "$$")
  
  return(list("formula" = formula, "adjr" = adjr))
}

trans.mtx = function(zdata, selected)
{
  row = selected[1,1] %>% as.numeric()
  col = selected[1,2] %>% as.numeric()
  
  year = 2016 + row
  sce = c("Base", "Adverse", "Severe")[col]
  z = zdata %>% filter(Year == year & scenario == sce) %>% select(value) %>% as.numeric()
  rho = Z$rho
  cml = data.prep(loan)$cml_cnt
  
  data = delta.transition(rho, z, cml)
  
  colnames(data) = c(row.names(data),"Default")

  return(data)
}

overall_df = function(zdata) {
  rho = Z$rho
  cml = data.prep(loan)$cml_cnt
  
  year = 2016
  actual = zdata %>% filter(Year == year & scenario == "Fitted") %>% select(value) %>% as.numeric()
  Current = delta.transition(rho, actual, cml)[,8]
  
  year = c(2017,2018,2019)
  sce = c("Base", "Adverse", "Severe")
  df_sce = NULL
  for (j in sce) {
    d = NULL
    for (i in year) {
      z = zdata %>% filter(Year == i & scenario == j) %>% select(value) %>% as.numeric()
      di = delta.transition(rho, z, cml)[,8]
      d = cbind(d,di)
    }
    df_sce = cbind(df_sce,apply(d,1,mean))
  }
  data = data.frame(Current,df_sce)
  colnames(data) = c("Current","Base (3-Year Average)","Adverse (3-Year Average)","Severe (3-Year Average)")
  return(data)
}
