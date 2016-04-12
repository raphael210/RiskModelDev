source('~/R/FactorModelDev/RiskModelDev.R', encoding = 'UTF-8', echo=TRUE)
RebDates <- getRebDates(as.Date('2009-12-31'),as.Date('2016-03-31'),'month')
TS <- getTS(RebDates,'EI000985')

riskfactorLists <- buildFactorLists(
  buildFactorList(factorFun = "gf.liquidity",factorDir = -1,factorNA = "median",factorStd = "norm"),
  buildFactorList(factorFun = "gf.ln_mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm"))

alphafactorLists1 <- buildFactorLists(
  buildFactorList("gf.NP_YOY",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_scissor_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.GG_NP_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.GG_OR_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_OCF",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_SCF_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_MLL_Q",factorStd="norm",factorNA = "median")
)
factorIDs <- c("F000003","F000004","F000006","F000007","F000008","F000009","F000010")
alphafactorLists2 <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
alphafactorLists <- c(alphafactorLists1,alphafactorLists2)

TSF <- getTSFQuick(TS,alphafactorLists,riskfactorLists)

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

optimizedwgt <- OptWgt(TSF,alphaf,Fcov,Delta,constr='Ind')

