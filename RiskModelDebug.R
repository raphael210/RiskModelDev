source('~/R/FactorModelDev/RiskModelDev.R', encoding = 'UTF-8', echo=TRUE)

library(RFactorModel)
RebDates <- getRebDates(as.Date('2015-12-31'),as.Date('2016-06-30'),rebFreq = 'month')
TS <- getTS(RebDates,'EI000300')

# added by qianmazi---test
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


