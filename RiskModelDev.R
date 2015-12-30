library(RFactorModel)
library(lubridate)
library(reshape2)
library(plyr)
library(stringr)

mp = modelPar.default()
mp <-setmodelPar.time(mp, begT=as.Date('2013-01-31'), endT=as.Date('2015-11-30'), rebFreq='month')
mp <- setmodelPar.univ(mp,indexID = 'EI000905')
factorLists <- buildFactorLists(
  buildFactorList("gf.NP_YOY"),
  buildFactorList("gf.G_scissor_Q"),
  buildFactorList("gf.GG_NP_Q"),
  buildFactorList("gf.GG_OR_Q"),
  buildFactorList("gf.G_OCF"),
  buildFactorList("gf.G_SCF_Q"),
  buildFactorList("gf.G_MLL_Q") ,
  factorStd="norm"
)
factorIDs <- c("F000001","F000003","F000004","F000006","F000007","F000008","F000009","F000010")
factorLists2 <- buildFactorLists_lcfs(factorIDs,
                                      factorStd="norm")
factorLists <- c(factorLists,factorLists2)
wgts<-rep(1,15)
# -- multi-factor 
mp <- modelPar.factor(mp,
                      factorFun="getMultiFactor",
                      factorPar=list(factorLists,wgts),
                      factorStd="norm",
                      factorDir=1,
                      factorName="multifactor")
ts <- Model.TS(mp)
tsfr <- Model.TSFR(mp)

tsrf <- tsfr[,c("nextRebalanceDate","stockID","periodrtn",sapply(factorLists, "[[", "factorName"))]
tsrf <- rename(tsrf, replace = c("nextRebalanceDate" = "date"))


add.sector <- function(TSRF){
  # get sectorID
  tss <- getSectorID(ts)
  tss$sector[is.na(tss$sector)] <- 'ESNULL'
  tss$value <- c(1)
  tss <- dcast(tss,date+stockID~sector,fill=0,value.var='value')
  
}




tsfrs <- merge(tsfr,tss,by=c('date','stockID'))
tsfrs <- tsfrs[,-c('InfoPublDate','EndDate','src','factorscore')]





