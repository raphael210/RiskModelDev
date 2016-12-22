library(RFactorModel)
library(PortfolioAnalytics)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ------------------------- Model setting -----------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modelPar <- modelPar.default()
modelPar <- modelPar.title(modelPar,modelID='000001',modelName='testmodel')
modelPar <- modelPar.univ(modelPar,indexID='EI000300')       # - "EI000300","EI000905","EI801344"
modelPar <- modelPar.time(modelPar,
                          begT = as.Date("2007-01-01"),
                          endT = as.Date("2012-12-31"),
                          rebFreq = "month",
                          shiftby = 0 )
modelPar <- modelPar.factor(modelPar,
                            factorFun="gf.PE_ttm",
                            factorPar=list(),
                            factorDir=-1,
                            factorStd = "norm",           # - "sectorNe","none","norm"
                            factorOutlier = 3,
                            factorNA = "median")
modelPar <- modelPar.dataclean(modelPar,                                
                               std.rtn = "none",              # - "sectorNe","none","norm"                
                               outlier.rtn = 3,
                               sectorSTD    = 9 ,
                               sectorLevel = 99)
str(modelPar)

# ----- backtesting Parametres setting ------------
backtestPar <- backtestPar.default()
backtestPar <- backtestPar.IC(backtestPar,stat="pearson")
backtestPar <- backtestPar.Ngroup(backtestPar,N=5)
backtestPar <- backtestPar.longshort(backtestPar,
                                     topN = 60,
                                     holdingEndT = as.Date("2012-12-31"),
                                     wgtType = "fvsqrt",          # - "eq","fv","fvsqrt","custom"
                                     wgt.sectorNe = TRUE,
                                     wgt.max =0.08,
                                     sectorSTD =9,
                                     sectorLevel =99,
                                     bmk = "EI000300",          # - "EI000300","EI000905","EI399984","EI801344"
                                     hedge.rebFreq = "month",
                                     hitFreq = "month" )
backtestPar <- backtestPar.fee(backtestPar,
                               secu = 0.000,
                               future = 0.0000 )
str(backtestPar)

# ----- plotting Parametres setting ----------------
plotPar <- plotPar.default()
plotPar <- plotPar.TSFRScatter(plotPar,Nbin="day")
plotPar <- plotPar.IC(plotPar,Nbin="quarter")
plotPar <- plotPar.Ngroup(plotPar,
                          Nbin="2 quarter",
                          stat="mean",
                          turnoverType="num" )
plotPar <- plotPar.longshort(plotPar,
                             bar.freq   = "month",
                             roll.width = 250,
                             roll.by    = 30 )
str(plotPar)


# ---- get the objects of TS,TSR,TSF,TSFR step by step ------------------
indexID <- 'EI000985'
begT <- as.Date('2006-01-01')
endT <- as.Date('2016-06-30')
rebFreq <- "month"
factorFun <- 'gf.ln_mkt_cap'
RebDates <- getRebDates(begT,endT,rebFreq)
TS <- getTS(RebDates,indexID=indexID)
IVR.tmp <- IVR[IVR$date %in% unique(TS$date),]
TSF <- merge(TS,IVR.tmp,by=c('date','stockID'),all.x = T)
colnames(TSF) <- c("date","stockID","factorscore")
TSFR <- getTSR(TSF)
TSFR <- TSFR[!is.na(TSFR$factorscore),]

TSFR <- getTSF(TSR,factorFun)


# ---- get the objects of TS,TSR,TSF,TSFR directly via modelPar -----------------------
RebDates <- Model.RebDates(modelPar)
TS <- Model.TS(modelPar)
TSR <- Model.TSR(modelPar,include.decay=TRUE)  #--#
TSF <- Model.TSF(modelPar)
TSFR <- Model.TSFR(modelPar,include.decay=F)
TSFRM <- Model.TSFRM(modelPar,include.decay=TRUE)

# ---- get the objects of TSF,TSFR via TS and modelPar -----------------------
TSF <- Model.TSF_byTS(TS,modelPar)
TSFR <- Model.TSF_byTS(TSR,modelPar)  #--#
TSFRM <- Model.TSFRM_byTSR(modelPar,TSR)








# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ------------------- Singlefactor backtesting  -----------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TSR <- Model.TSR(modelPar,include.decay=TRUE)   #--#

TSFR <- Model.TSF_byTS(TSR,modelPar)  #--#

# --- backtest.IC 
IC.table <- table.IC(TSFR,backtestPar=backtestPar)
IC.table.decay <- table.IC.decay(TSFR,backtestPar=backtestPar)
IC.plot <- chart.IC(TSFR,plotPar=plotPar)
IC.plot.decay <- chart.IC.decay(TSFR,plotPar=plotPar)

# --- backtest.Ngroup
Ngroup.table.overall <- table.Ngroup.overall(TSFR,backtestPar=backtestPar)
Ngroup.table.spread <- table.Ngroup.spread(TSFR,backtestPar=backtestPar)
Ngroup.plot.overall <- chart.Ngroup.overall(TSFR,plotPar=plotPar)
Ngroup.plot.seri_point <- chart.Ngroup.seri_point(TSFR,plotPar=plotPar)
Ngroup.plot.seri_bar <- chart.Ngroup.seri_bar(TSFR,plotPar=plotPar)
Ngroup.plot.seri_line <- chart.Ngroup.seri_line(TSFR,plotPar=plotPar)
Ngroup.plot.spread <- chart.Ngroup.spread(TSFR,plotPar=plotPar)  # return and print
Ngroup.plot.turnover <- chart.Ngroup.turnover(TSFR,plotPar=plotPar)

re <- list(
  factorFun = getmodelPar.factor(modelPar,"factorFun"),
  factor.list = getmodelPar.factor(modelPar),  
  IC.table = IC.table,
  IC.table.decay = IC.table.decay,
  IC.plot = IC.plot,
  IC.plot.decay = IC.plot.decay,  
  Ngroup.table.overall = Ngroup.table.overall,
  Ngroup.table.spread = Ngroup.table.spread,
  Ngroup.plot.seri_point = Ngroup.plot.seri_point,
  Ngroup.plot.seri_bar = Ngroup.plot.seri_bar,
  Ngroup.plot.seri_line = Ngroup.plot.seri_line,
  Ngroup.plot.spread = Ngroup.plot.spread,
  Ngroup.plot.turnover = Ngroup.plot.turnover
)

if(!simple){ # --- tables.longshort    
  # -- getPort
  Port.L <- getPort(TSFR,dir="top",backtestPar=backtestPar)
  Port.S <- getPort(TSFR,dir="bottom",backtestPar=backtestPar)
  Port.L <- addwgt2port(Port.L,backtestPar=backtestPar)
  Port.S <- addwgt2port(Port.S,backtestPar=backtestPar)
  Port.L <- port.substitute(Port.L,TSFR,dir="long",backtestPar=backtestPar)
  Port.S <- port.substitute(Port.S,TSFR,dir="short",backtestPar=backtestPar)
  PB.L <- port.backtest(Port.L,dir="long",backtestPar=backtestPar)
  PB.S <- port.backtest(Port.S,dir="short",backtestPar=backtestPar)
  # -- longshort
  rtn.LSH <- getrtn.LSH(PB.L,PB.S,backtestPar=backtestPar)
  longshort.tables <- tables.longshort(rtn.LSH,backtestPar=backtestPar)
  longshort.plot.summary <- chart.longshort.summary(rtn.LSH,plotPar=plotPar) # return and print
  longshort.plot.rolling <- chart.longshort.rolling(rtn.LSH,plotPar=plotPar) # # return and print
  # -- longbmk
  rtn.LBH <- getrtn.LBH(PB.L,backtestPar=backtestPar)
  longbmk.tables <- tables.longshort(rtn.LBH,backtestPar=backtestPar)
  longbmk.plot.summary <- chart.longshort.summary(rtn.LBH,plotPar=plotPar)   # return and print
  longbmk.plot.rolling <- chart.longshort.rolling(rtn.LBH,plotPar=plotPar)   # return and print
  
  re <- c(re,
          longshort.tables,
          longshort.plot.summary,
          longshort.plot.rolling,
          longbmk.tables,
          longbmk.plot.summary,
          longbmk.plot.rolling)
}




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ------------------ Multifactor comparison -----------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TSR <- Model.TSR(modelPar,include.decay=TRUE)  #--#

factorLists <- list(
  buildFactorList(factorFun="getFactor.pct_chg_per",
                  factorPar=list(N=60),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.float_cap",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.PE_ttm",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.PB_mrq",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.F_NP_chg",
                  factorPar=list(span="w13"),
                  factorDir=1),
  buildFactorList(factorFun="getFactor.F_target_rtn",
                  factorPar=list(con_type="1,2"),
                  factorDir=1),
  buildFactorList(factorFun="getFactor.F_rank_chg",
                  factorPar=list(lag=60,con_type="1,2"),
                  factorDir=1),
  buildFactorList(factorFun="getFactor.F_PE",
                  factorPar=list(con_type="1"),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.F_ROE",
                  factorPar=list(con_type="1"),
                  factorDir=1)
)
modelPars <- llply(factorLists,function(x) modelPar.factor(modelPar,factorList=x))
TSFRMs <- lapply(modelPars, Model.TSFRM_byTSR, TSR=TSR)
names(TSFRMs) <- sapply(modelPars, getmodelPar.factor, item_sub="factorName")


# --- MultiFactors model through lcfs
# factorIDs=c("F000008","F000001","F000006","F000003","F000004","F000009","F000010","F000011")
# factorLists <- buildFactorLists.lcfs(factorIDs)
# modelPars <- llply(factorLists,function(x) modelPar.factor(modelPar,factorList=x))
# TSFRMs <- lapply(modelPars, Model.TSFRM)
# names(TSFRMs) <- sapply(modelPars, getmodelPar.factor, item_sub="factorName")


# --- summary report
wdrpt.MC(TSFRMs,backtestPar,plotPar)


# --- comparison
NMs <- laply(TSFRMs,function(x) getmodelPar.factor(x$modelPar,"factorName"))
names(TSFRMs) <- NMs
# --- IC.table
IC.table <- laply(TSFRMs,function(x) table.IC(x$TSFR,backtestPar=backtestPar)) 
rownames(IC.table) <- NMs
# --- spread.table
spread.table <- laply(TSFRMs,function(x) table.Ngroup.overall(x$TSFR,backtestPar=backtestPar)[,1])
rownames(spread.table) <- NMs
# --- IC.multicharts
IC.multicharts <- MC.chart.IC(TSFRMs=TSFRMs,plotPar=plotPar)
# --- IC.multicharts.decay
IC.multicharts.decay <- MC.chart.IC.decay(TSFRMs=TSFRMs,plotPar=plotPar)
# --- IC.corrplot
IC.corrplot <- MC.chart.IC.corr(TSFRMs=TSFRMs)










# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ------------------ Multifactor model -----------------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TSR <- Model.TSR(modelPar,include.decay=TRUE)  #--#

factorLists <- list(
  buildFactorList(factorFun="getFactor.pct_chg_per",
                  factorPar=list(N=60),
                  factorDir=-1),  
  buildFactorList(factorFun="getFactor.float_cap",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.PB_mrq",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.F_NP_chg",
                  factorPar=list(span="w13"),
                  factorDir=1),
  buildFactorList(factorFun="getFactor.F_target_rtn",
                  factorPar=list(con_type="1,2"),
                  factorDir=1),
  buildFactorList(factorFun="getFactor.F_rank_chg",
                  factorPar=list(lag=60,con_type="1,2"),
                  factorDir=1),
  buildFactorList(factorFun="getFactor.F_PE",
                  factorPar=list(con_type="1"),
                  factorDir=-1),
  buildFactorList(factorFun="getFactor.F_ROE",
                  factorPar=list(con_type="1"),
                  factorDir=1)
)

wgts <- c(0.1,0.1,0.1,0.2,0.2,0.1,0.1,0.1)
modelPar <- modelPar.factor(modelPar,
                            factorFun="getMultiFactor",
                            factorPar=list(factorLists,wgts,sectorSTD=9,sectorLevel=99),
                            factorStd="sectorNe",
                            factorDir=1,
                            factorName="multifactor")

TSFR <- Model.TSF_byTS(TSR,modelPar)   #--#


# --- MultiFactors model through lcfs
factorIDs=c("F000008","F000001","F000006","F000003","F000004","F000009","F000010","F000011")
factorLists <- buildFactorLists.lcfs(factorIDs)
wgts <- c(0.1, 0.3, 0.1, 0.3, 0, 0.1, 0, 0.1)
modelPar <- modelPar.factor(modelPar,
                            factorFun="getMultiFactor",
                            factorPar=list(factorLists,wgts,sectorSTD=9,sectorLevel=99),
                            factorStd="sectorNe",
                            factorDir=1,
                            factorName="multifactor")
TSFR <- Model.TSFR(modelPar)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ------------------ Newest Portfolio -----------------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# -----
idx <- "SH000905"       # - "EI000300","EI000905"
DT <- as.Date("2013-02-28")
# -----
modelPar <- modelPar.default()
modelPar <- modelPar.univ(modelPar,indexID=idx)
modelPar <- modelPar.time(modelPar,
                          begT = DT,
                          endT = DT,
                          rebFreq = "day",
                          shiftby = 0 )
modelPar <- modelPar.factor(modelPar,
                            factorFun="getMultiFactor",
                            factorPar=list(factorLists,wgts,
                                           sectorSTD=9,
                                           sectorLevel = if(idx=="EI000300") 99 else 1),
                            factorStd="sectorNe",
                            factorDir=1,
                            factorName="multifactor")
modelPar <- modelPar.dataclean(modelPar,                                
                               std.rtn = "none",              # - "sectorNe","none","norm"                
                               outlier.rtn = 3,
                               sectorSTD    = 9 ,
                               sectorLevel = if(idx=="EI000300") 99 else 1 )
# ----- 
backtestPar <- backtestPar.default()
backtestPar <- backtestPar.longshort(backtestPar,
                                     topN = if(idx=="EI000300") 60 else 80,
                                     wgtType = "fvsqrt",          # - "eq","fv","fvsqrt","custom"
                                     wgt.sectorNe = TRUE,
                                     wgt.max =0.08,
                                     sectorSTD =9,
                                     sectorLevel = if(idx=="EI000300") 99 else 1,
                                     bmk = idx       # - "EI000300","EI399984"
)
# -----
TSF.n <- Model.TSF(modelPar)
# -- getPort
Port.L <- getPort(TSF.n,dir="top",backtestPar=backtestPar)
Port.S <- getPort(TSF.n,dir="bottom",backtestPar=backtestPar)
Port.L <- addwgt2port(Port.L,backtestPar=backtestPar)
Port.S <- addwgt2port(Port.S,backtestPar=backtestPar)
Port.L <- port.substitute(Port.L,TSFR,dir="long",backtestPar=backtestPar)
Port.S <- port.substitute(Port.S,TSFR,dir="short",backtestPar=backtestPar)
Port.L <- subset(Port.L,select=c("date","stockID","sector","factorscore","wgt"))
Port.S <- subset(Port.S,select=c("date","stockID","sector","factorscore","wgt"))
filename <- paste(getmodelPar.univ(modelPar,"indexID"),as.character(getmodelPar.time(modelPar,"endT"),"%Y%m%d"),sep="_")
filename.L <- paste(paste("longPort",filename,sep="_"),".csv",sep="")
filename.S <- paste(paste("shortPort",filename,sep="_"),".csv",sep="")
dirname <- "portdata"
write.csv(Port.L,file.path(dirname,filename.L),row.names=FALSE)
write.csv(Port.S,file.path(dirname,filename.S),row.names=FALSE)







library(RFactorModel)
mp <- modelPar.default()
mp <- setmodelPar.time(mp,begT=as.Date("2009-12-31"),endT=as.Date("2016-06-30"),rebFreq = "month")
#CT_FactorLists()
mp <- setmodelPar.univ(mp,indexID = 'EI000905')
mp <- setmodelPar.factor(mp,factorFun="gf.beta")
mp
TSFR <- Model.TSFR(modelPar=mp,include.decay=F)

TS <- TSFR[,c("date","stockID")]
TSR <- TSFR[,c("date","stockID","nextRebalanceDate","periodrtn")]
TSF <- getTSF(TS,factorFun='gf.ln_mkt_cap', factorPar=list(),factorStd = "norm",factorNA = 'median')
TSFR <- merge(TSF,TSR,by=c("date","stockID"))

# single factor backtest
chart.IC(TSFR)
#chart.IC.decay(TSFR)
chart.Ngroup.overall(TSFR,N = 5)
chart.Ngroup.seri_bar(TSFR,Nbin="year")
chart.Ngroup.seri_line(TSFR,N=5)
chart.Ngroup.seri_point(TSFR)
chart.Ngroup.seri_point(TSFR,Nbin="quarter")
chart.Ngroup.spread(TSFR)
table.Ngroup.overall(TSFR)
table.Ngroup.spread(TSFR)
table.Ngroup.overall(TSFR,N=8)


#  port backtest
PB.L <- getPB(TSFR,topN=60,wgtType="fvsqrt",holdingEndT=as.Date("2013-12-31"),dir="long")
PB.S <- getPB(TSFR,topN=60,wgtType="fvsqrt",holdingEndT=as.Date("2013-12-31"),dir="short")
rtn.LSH <- getrtn.LSH(PB.L,PB.S)
tables.longshort(rtn.LSH)
chart.longshort.summary(rtn.LSH)
chart.longshort.summary(rtn.LSH,bar.freq="quarter")
rtn.LBH <- getrtn.LBH(PB.L)
chart.longshort.summary(rtn.LBH)
chart.longshort.rolling(rtn.LBH)
head(rtn.LBH)
tail(rtn.LBH)


# multi factors backtest ,corr test
mp <- modelPar.default()
mp <- setmodelPar.time(mp,begT=as.Date("2009-12-31"),endT=as.Date("2016-03-31"),rebFreq = "month")
mp <- setmodelPar.univ(mp,indexID = 'EI000985')
factorLists1 <- buildFactorLists(
  buildFactorList("gf.NP_YOY",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_scissor_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.GG_NP_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.GG_OR_Q",factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_SCF_Q", factorStd="norm",factorNA = "median"),
  buildFactorList("gf.G_MLL_Q",factorStd="norm",factorNA = "median")
)
factorIDs <- c("F000003","F000004","F000008","F000009","F000010")
factorLists2 <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
factorLists <- c(factorLists1,factorLists2)
# -- multi-factor 
mps <- getMPs_FactorLists(factorLists,modelPar=mp)
TSR <- Model.TSR(mp)
TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)
MC.chart.IC.corr(TSFRs)
MC.table.IC(TSFRs)
MC.chart.IC(TSFRs)

