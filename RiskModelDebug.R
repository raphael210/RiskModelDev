source('~/R/FactorModelDev/RiskModelDev.R', encoding = 'UTF-8', echo=TRUE)
<<<<<<< HEAD
RebDates <- getRebDates(as.Date('2011-12-31'),as.Date('2016-07-31'),rebFreq = 'week')
TS <- getTS(RebDates,'EI000985')
=======

library(RFactorModel)
RebDates <- getRebDates(as.Date('2015-12-31'),as.Date('2016-06-30'),rebFreq = 'month')
TS <- getTS(RebDates,'EI000300')
>>>>>>> 11121ced2cdf34ceb2bdf389e67280a80c2e2f19

#alpha factor setting 
alphafactorLists <- buildFactorLists(
  buildFactorList("gf.NP_YOY",factorStd="norm")
)
factorIDs <- c("F000003","F000008")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm")
alphafactorLists <- c(alphafactorLists,tmp)

#risk factor setting 
riskfactorLists <- buildFactorLists(
  buildFactorList(factorFun = "gf.mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm")
)
factorIDs <- c("F000006")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
riskfactorLists <- c(riskfactorLists,tmp)

data <- calcfresbyTS(TS,alphafactorLists,riskfactorLists,"glm",sectorAttr = list(33,1),dure = months(1))





<<<<<<< HEAD
factorLists <- c(alphafactorLists,riskfactorLists)
for(i in 1:length(factorLists)){
  factorFun <- factorLists[[i]]$factorFun
  factorPar <- factorLists[[i]]$factorPar
  factorName <- factorLists[[i]]$factorName
  factorDir <- factorLists[[i]]$factorDir
  factorOutlier <- factorLists[[i]]$factorOutlier
  factorNA <- factorLists[[i]]$factorNA    
  factorStd <- factorLists[[i]]$factorStd 
  sectorAttr  <- factorLists[[i]]$sectorAttr
  cat(paste("Function getMultiFactor: getting the score of factor",factorName,"....\n"))
  # ---- get the raw factorscore
  TSF <- getRawFactor(TS,factorFun,factorPar) 
  # ---- adjust the direction (of the "single-factor-score")
  TSF$factorscore <- TSF$factorscore*factorDir
  # ---- deal with the outliers (of the "single-factor-score")
  TSF <- RFactorModel:::factor.outlier(TSF,factorOutlier)
  # ---- standardize the factorscore (of the "single-factor-score")
  if(factorStd =="none"){
    warning(paste("'factorStd' of factor",factorName, "is 'none'. It might make mistake when compute the multi-factorscore!"))
  }
  TSF <- RFactorModel:::factor.std(TSF,factorStd,sectorAttr)  
  # ---- deal with the missing values (of the "single-factor-score")
  TSF <- RFactorModel:::factor.na(TSF,factorNA)
  
  TSF <- renameCol(TSF,"factorscore",factorName)
  if(i==1L){
    re <- TSF
  } else {
    re <- merge(re,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
  }
}


data <- calcfres(TSF,alphafactorLists,riskfactorLists,regresstype = 'glm')
=======
>>>>>>> 11121ced2cdf34ceb2bdf389e67280a80c2e2f19
TSFR <- data[[1]]
alphaf <- data[[2]]
riskf <- data[[3]]
residual <- data[[4]]

data1 <- calcFDelta(riskf,residual)
Fcov <- data1[[1]]
Delta <- data1[[2]]

alphaf <- calcAlphaf(alphaf)

#clean data
TSF <- subset(TSFR,date>=min(Fcov$date),select = -c(nextRebalanceDate,periodrtn))
alphaf <- alphaf[alphaf$date>=min(Fcov$date),]

tmp <- sapply(riskfactorLists,'[[','factorName')
riskfexp=data.frame(sector=tmp,lb=rep(-0.05,length(tmp)),ub=rep(0.05,length(tmp)))
riskfexp[riskfexp$sector=='ln_mkt_cap_','lb'] <- -1
riskfexp[riskfexp$sector=='ln_mkt_cap_','ub'] <- 0.1

optimizedwgt <- OptWgt(TSF,alphaf,Fcov,Delta,constr='IndSty',benchmark='EI000905',riskavr = 100,indfexp=0.05,riskfexp)


#calculate portfolio return
optpot <- optimizedwgt[optimizedwgt$wgtopt>0,c('date','stockID','wgtopt')]
colnames(optpot) <- c("date","stockID","wgt")
PB <- port.backtest(optpot,holdingEndT = Sys.Date()-3)
re <- getrtn.LBH(PB,"EI000905",fee.long = 0.001)
ggplot.WealthIndex(re)
rtn.summary(re)


# update local data base
library(quantbox)
library(lubridate)
tsInclude()
tsConnect()
system.time(lcdb.update())
lcfs.update()
fix.lcdb.swindustry()


