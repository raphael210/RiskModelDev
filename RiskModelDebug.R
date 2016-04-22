source('~/R/FactorModelDev/RiskModelDev.R', encoding = 'UTF-8', echo=TRUE)
RebDates <- getRebDates(as.Date('2009-12-31'),as.Date('2015-08-31'),'month')
TS <- getTS(RebDates,'EI000905')

riskfactorLists <- buildFactorLists(
  buildFactorList(factorFun = "gf.liquidity",factorDir = -1,factorNA = "median",factorStd = "norm"),
  buildFactorList(factorFun = "gf.ln_mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm"))

alphafactorLists1 <- buildFactorLists(
  buildFactorList("gf.NP_YOY",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_scissor_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.GG_NP_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.GG_OR_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_SCF_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_MLL_Q",factorStd="norm",factorNA = "median")
)
factorIDs <- c("F000003","F000004","F000008","F000009","F000010")
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

tmp <- sapply(riskfactorLists,'[[','factorName')
riskfexp=data.frame(sector=tmp,lb=rep(-0.1,length(tmp)),ub=rep(0.1,length(tmp)))
riskfexp[riskfexp$sector=='ln_mkt_cap_','lb'] <- -10
riskfexp[riskfexp$sector=='ln_mkt_cap_','ub'] <- 0.3

optimizedwgt <- OptWgt(TSF,alphaf,Fcov,Delta,constr='IndSty',benchmark='EI000905',riskavr = 10,indfexp=0.05,riskfexp)


#calculate portfolio return
optpot <- optimizedwgt[optimizedwgt$wgtopt>0,c('date','stockID','wgtopt')]
TSR <- TSFR[,c('date','stockID','nextRebalanceDate','periodrtn')]
optpot <- merge(optpot,TSR,by=c('date','stockID'),all.x = T)
optpot <- optpot[!is.na(optpot$nextRebalanceDate),]
dates <- unique(optpot$nextRebalanceDate)
for(i in dates){
  tmp <- optpot[optpot$nextRebalanceDate==i,]
  tmp.df <- data.frame(date=i,rtn=t(as.matrix(tmp$wgtopt)) %*% as.matrix(tmp$periodrtn))
  if(i==dates[1]){
    potrtn <- tmp.df
  }else{
    potrtn <- rbind(potrtn,tmp.df)
  }
  
}
potrtn$date <- as.Date(potrtn$date)
potrtn <- as.xts(potrtn$rtn,order.by = potrtn$date)
ggplot.WealthIndex(potrtn)
re <- getrtn.bmk(potrtn, bmk = "EI000905")
ggplot.WealthIndex(re)

b <- data.frame(stockID=rownames(alphamat))
b <- merge(b,benchmarkdata[,2:3],by='stockID',all.x = T)
b[is.na(b$wgt),'wgt'] <- 0
b$wgt <- b$wgt/sum(b$wgt)
b <- as.matrix(b$wgt)

c(t(Amat)%*%b)
c(t(Amat)%*%b >bvec)
ret%*%b
t(b)%*%Dmat%*%b
(0.5*t(b)%*%Dmat%*%b)/(-1*ret%*%b)

b <- as.matrix(res$solution)
b[abs(b)<0.001] <- 0

