source('~/R/FactorModelDev/RiskModelDev.R', encoding = 'UTF-8', echo=TRUE)
RebDates <- getRebDates(as.Date('2009-12-31'),as.Date('2016-06-30'),rebFreq = 'week')
TS <- getTS(RebDates,'EI000985')

# added by qianmazi---test
#alpha factor setting 
alphafactorLists <- buildFactorLists(
  buildFactorList("gf.NP_YOY",factorStd="norm")
)
factorIDs <- c("F000003","F000008","F000009","F000012","F000013","F000014","F000017")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm")
alphafactorLists <- c(alphafactorLists,tmp)

#risk factor setting 
riskfactorLists <- buildFactorLists(
  buildFactorList(factorFun = "gf.ln_mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm")
)
factorIDs <- c("F000006","F000015","F000016")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
riskfactorLists <- c(riskfactorLists,tmp)


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
  TSF <- factor.outlier(TSF,factorOutlier)
  # ---- standardize the factorscore (of the "single-factor-score")
  if(factorStd =="none"){
    warning(paste("'factorStd' of factor",factorName, "is 'none'. It might make mistake when compute the multi-factorscore!"))
  }
  TSF <- factor.std(TSF,factorStd,sectorAttr)  
  # ---- deal with the missing values (of the "single-factor-score")
  if(factorNA=="na") factorNA <- "median"   # -- there should not be a missing value in the multi-factor-scores matrix !
  TSF <- factor.na(TSF,factorNA)
  
  TSF <- renameCol(TSF,"factorscore",factorName)
  if(i==1L){
    re <- TSF
  } else {
    re <- merge(re,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
  }
}


data <- calcfres(TSF,alphafactorLists,riskfactorLists,regresstype = 'glm')
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
tsInclude()
tsConnect()
lcdb.update()
add.index.lcdb(indexID="EI801003")
add.index.lcdb(indexID="EI000985")
fix.lcdb.swindustry()


