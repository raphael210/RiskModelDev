source('~/R/FactorModelDev/RiskModelDev.R', encoding = 'UTF-8', echo=TRUE)
RebDates <- getRebDates(as.Date('2009-12-31'),as.Date('2016-06-30'),'month')
TS <- getTS(RebDates,'EI000985')

riskfactorLists <- buildFactorLists(
  buildFactorList(factorFun = "gf.liquidity",factorDir = -1,factorNA = "median",factorStd = "norm"),
  buildFactorList(factorFun = "gf.ln_mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm"),
  buildFactorList(factorFun = "gf.beta",factorDir = -1,factorNA = "median",factorStd = "norm")
  )
factorIDs <- c("F000006")
riskfactorLists2 <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
riskfactorLists <- c(riskfactorLists,riskfactorLists2)

alphafactorLists <- buildFactorLists(
  buildFactorList("gf.NP_YOY",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_scissor_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.GG_OR_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_SCF_Q", factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_MLL_Q",factorStd="norm",factorNA = "median")
)
factorIDs <- c("F000003","F000004","F000008","F000009","F000010")
alphafactorLists2 <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
alphafactorLists <- c(alphafactorLists,alphafactorLists2)

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
lcdb.update.QT_FactorScore_amtao()
fix.lcdb.swindustry()

system.time(lcfs.add(factorFun="gf.F_rank_chg",factorPar="lag=60,con_type=\"1,2\"", factorDir=1, factorID="F000009"))
