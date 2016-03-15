#load packages
suppressMessages(library(RFactorModel))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))
suppressMessages(library(plyr))
suppressMessages(library(stringr))
suppressMessages(library(nloptr))

#' add.index.lcdb
#'
#' Add a index to local database
#' @author Andrew Dow
#' @param indexID is the indexID
#' @return nothing
#' @examples 
#' add.index.lcdb(indexID="000985.SH")
add.index.lcdb <- function(indexID="000985.SH"){
  #check whether the index in local db
  qr <- paste("select * from SecuMain where ID="
              ,str_c("'EI",str_sub(indexID,1,6),"'"),sep="")
  re <- dbGetQuery(db.local(),qr)
  if(nrow(re)>0) return("Already in local database!")
  
  #part 1 update local SecuMain
  qr <- paste("select ID,InnerCode,CompanyCode,SecuCode,SecuAbbr,SecuMarket,  
              ListedSector,ListedState,JSID 'UpdateTime',SecuCode 'StockID_TS',
              SecuCategory,ListedDate,SecuCode 'StockID_wind'
              from SecuMain WHERE SecuCode=",
              str_c("'",str_sub(indexID,start = 1,end = 6),"'"),
              " and SecuCategory=4",sep='')
  re <- sqlQuery(db.jy(),qr)
  re <- transform(re,ID=str_c("EI",str_sub(indexID,1,6)),
                  SecuCode=str_pad(SecuCode,6,pad='0'),
                  StockID_TS=str_c(str_sub(indexID,8,-1),str_sub(indexID,1,6)),
                  StockID_wind=indexID,
                  ListedDate=rdate2int(ListedDate))
  dbWriteTable(db.local(),"SecuMain",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
  
  #part 2 update local LC_IndexComponent
  suppressMessages(library(timeDate))
  suppressMessages(library(WindR))
  w.start(showmenu = F)
  dates <- seq.Date(as.Date('2004-12-01'),Sys.Date(),by="6 month")
  dates <- as.Date(timeNthNdayInMonth(dates, nday = 5, nth = 2, format = "%Y-%m-%d"))
  dates <- trday.nearby(dates,-1)
  
  subfun <- function(date){
    tmp<-w.wset('SectorConstituent',date=date,windcode=indexID)$Data
    return(tmp)
  }
  oridata<-ldply(dates,subfun)
  oridata$date <- w.asDateTime(oridata$date,asdate = T)
  result <- ddply(oridata, "wind_code", summarise,InDate=min(date),OutDate=max(date))
  dates <- data.frame(OutDate=dates,new=c(dates[2:length(dates)],NA))
  result <- merge(result,dates,by = 'OutDate')
  result <- transform(result,indexID=c(str_c("EI",str_sub(indexID,1,6))),
                      Flag=ifelse(is.na(new),1,0),UpdateTime=Sys.time())
  result <- result[,c("indexID","wind_code","InDate","new","Flag","UpdateTime")]
  colnames(result) <- c("indexID","SecuID","InDate","OutDate","Flag","UpdateTime")
  result <- transform(result,SecuID=str_sub(str_c('EQ',SecuID,sep = ''),start = 1,end = 8),
                      InDate=rdate2int(InDate),OutDate=rdate2int(OutDate))
  dbWriteTable(db.local(),"LC_IndexComponent",result,overwrite=FALSE,append=TRUE,row.names=FALSE)
  return("Done!")
}

#' build.liquid.factor
#'
#' create liquidity factor in local db and update the factor
#' @author Andrew Dow
#' @param type indicates whether to create the new factor or to update the factor.
#' @return nothing
#' @examples 
#' build.liquid.factor("new")
#' build.liquid.factor("update")
build.liquid.factor <- function(type=c("new","update")){
  time <- Sys.time()
  type <- match.arg(type)
  if(type=='new'){
    qr <- "select t.ID,t.TradingDay,t.TurnoverVolume,t.NonRestrictedShares 
    from QT_DailyQuote t
    where t.TradingDay>=20050101"
    re <- dbGetQuery(db.local(),qr)
    
    re$TurnoverRate <- re$TurnoverVolume/(re$NonRestrictedShares*10000)
    re <- re[,c("ID","TradingDay","TurnoverRate")]
    re <- arrange(re,ID,TradingDay)
    re$TurnoverRate <-abs(re$TurnoverRate) 
    
    tmp <- as.data.frame(table(re$ID))
    tmp <- tmp[tmp$Freq>=21,]
    tmp <- tmp[substr(tmp$Var1, 1,3) %in% c('EQ0',"EQ6","EQ3"),]
    re <- re[re$ID %in% tmp$Var1,]
    
    result <- ddply(re,"ID",mutate,STOM=rollsum(TurnoverRate,21,fill=NA,align = 'right'))
    result <- subset(result,!is.na(result$STOM))
    result$STOM <- log(result$STOM)
    result[result$TurnoverRate==0,"STOM"] <- NA
    result <- melt(result,id=c("ID","TradingDay"))
    colnames(result) <- c("ID","TradingDay","FactorName","FactorScore")
    dbWriteTable(db.local(),"QT_FactorScore_Liquidity",result)
  }else{
    date <- dbGetQuery(db.local(),"select max(TradingDay) 'date' from QT_FactorScore_Liquidity")
    date <- date$date
    
    qr <- paste("select t.ID,t.TradingDay,t.TurnoverVolume,t.NonRestrictedShares 
    from QT_DailyQuote t
    where t.TradingDay>=",rdate2int(intdate2r(date)-60))
    re <- sqlQuery(db.quant(),qr)
    
    re$TurnoverRate <- re$TurnoverVolume/(re$NonRestrictedShares*10000)
    re <- re[,c("ID","TradingDay","TurnoverRate")]
    re <- arrange(re,ID,TradingDay)
    re$TurnoverRate <-abs(re$TurnoverRate) 
    
    tmp <- as.data.frame(table(re$ID))
    tmp <- tmp[tmp$Freq>=21,]
    tmp <- tmp[substr(tmp$Var1, 1,3) %in% c('EQ0',"EQ6","EQ3"),]
    re <- re[re$ID %in% tmp$Var1,]
    
    result <- ddply(re,"ID",mutate,STOM=rollsum(TurnoverRate,21,fill=NA,align = 'right'))
    result <- subset(result,!is.na(result$STOM))
    result$STOM <- log(result$STOM)
    result <- subset(result,TradingDay>date)
    result[result$TurnoverRate==0,"STOM"] <- NA
    result <- melt(result,id=c("ID","TradingDay"))
    colnames(result) <- c("ID","TradingDay","FactorName","FactorScore")
    dbWriteTable(db.local(),"QT_FactorScore_Liquidity",result,overwrite=FALSE,append=TRUE,row.names=FALSE)
  }
  print(Sys.time()-time)
}


#' gf.liquidity
#'
#' get liquidity factor in local db
#' @author Andrew Dow
#' @param TS is a TS object.
#' @return a TSF object
#' @examples 
#' gf.liquidity(TS)
gf.liquidity <- function(TS){
  check.TS(TS)  
  TS$date <- rdate2int(TS$date)
  con <- db.local()
  dbWriteTable(con,name="yrf_tmp",value=TS[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
  qr <-"select a.*,b.FactorScore 'factorscore'
  from yrf_tmp a left join QT_FactorScore_Liquidity b
  on a.date=b.TradingDay and a.stockID=b.ID and b.FactorName='STOM'"
  re <- dbGetQuery(con,qr)
  dbDisconnect(con)  
  re <- merge.x(TS,re,by=c("date","stockID"))  
  re <- transform(re, date=intdate2r(date))   
  return(re)
}


#' gf.ln_mkt_cap
#'
#' get ln(market_value) in local db
#' @author Andrew Dow
#' @param TS is a TS object.
#' @return a TSF object
#' @examples 
#' gf.ln_mkt_cap(TS)
gf.ln_mkt_cap <- function(TS){
  TSF <- gf.mkt_cap(TS)
  TSF$factorscore <- ifelse(is.na(TSF$factorscore),NA,log(TSF$factorscore))
  return(TSF)
}




#' calcfres
#'
#' calculate factor return f data and the residual data
#' @author Andrew Dow
#' @param TS is a TS object.
#' @param alphafactorLists is a set of alpha factors for regressing.
#' @param riskfactorLists is a set of risk factors for regressing.
#' @param regresstype choose the regress type,the default type is glm.
#' @return a list,contains TSFR,alphaf,riskf,residual
#' @examples 
#' RebDates <- getRebDates(as.Date('2009-12-31'),as.Date('2015-12-31'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' alphafactorLists <- buildFactorLists(
#'  buildFactorList("gf.NP_YOY",factorStd="norm",factorNA = "median"),
#'  buildFactorList("gf.G_scissor_Q",factorStd="norm",factorNA = "median"),
#'  buildFactorList("gf.GG_NP_Q",factorStd="norm",factorNA = "median"),
#'  buildFactorList("gf.GG_OR_Q",factorStd="norm",factorNA = "median"),
#'  buildFactorList("gf.G_OCF",factorStd="norm",factorNA = "median"),
#'  buildFactorList("gf.G_SCF_Q",factorStd="norm",factorNA = "median"),
#'  buildFactorList("gf.G_MLL_Q",factorStd="norm",factorNA = "median"))
#' riskfactorLists <- buildFactorLists(
#'  buildFactorList(factorFun = "gf.liquidity",factorDir = -1,factorNA = "median",factorStd = "norm"),
#'  buildFactorList(factorFun = "gf.ln_mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm"))
#' calcfres(TS,alphafactorLists,riskfactorLists)

calcfres <- function(TS,alphafactorLists,riskfactorLists,regresstype=c('glm','lm')){
  ptm <- proc.time()
  regresstype <- match.arg(regresstype)
  
  factorLists <- c(alphafactorLists,riskfactorLists)
  cat("getting factorscore......","\n")
  wgts <- rep(1/length(factorLists),length(factorLists))
  TSF <- getMultiFactor(TS,factorLists,wgts)
  
  TSF <- subset(TSF,select = -factorscore)
  if("NP_YOY" %in% colnames(TSF)) TSF <- subset(TSF,select = -c(InfoPublDate,EndDate,src,NP_LYCP))
  
  cat("getting sector id......","\n")
  TSS <- getSectorID(TS)
  TSS[is.na(TSS$sector),"sector"] <- "ESNone"
  TSS <- dcast(TSS,date+stockID~sector,length,fill=0)
  
  cat("getting period return......","\n")
  TSR <- getTSR(TS)
  
  #merge data
  TSF <- merge(TSF,TSS,by =c("date","stockID"))
  TSFR <- merge(TSF,TSR,by =c("date","stockID"))
  
  cat("calculating f and residual......","\n")
  if(regresstype=='glm'){
    #get liquid market value
    TSFv <- getTSF(TS,'gf.float_cap',factorStd="norm",factorNA = "median")
    TSFRv <- merge(TSFR,TSFv,by =c("date","stockID"))
    dates <- unique(TSFRv$date)
    for(i in 1:(length(dates)-1)){
      tmp.tsfr <- TSFRv[TSFRv$date==dates[i],]
      tmp.x <- as.matrix(tmp.tsfr[,-c(1,2,ncol(tmp.tsfr)-2,ncol(tmp.tsfr)-1,ncol(tmp.tsfr))])
      tmp.x <- subset(tmp.x,select = (colnames(tmp.x)[colSums(tmp.x)!=0]))
      tmp.r <- as.matrix(tmp.tsfr[,"periodrtn"])
      tmp.r[is.na(tmp.r)] <- mean(tmp.r,na.rm = T)
      tmp.w <- as.matrix(tmp.tsfr[,"factorscore"])
      tmp.w <- diag(c(tmp.w),length(tmp.w))
      tmp.f <- solve(crossprod(tmp.x,tmp.w) %*% tmp.x) %*% crossprod(tmp.x,tmp.w) %*% tmp.r
      tmp.residual <- tmp.r-tmp.x %*% tmp.f
      if(i==1){
        f=data.frame(date=dates[i+1],fname=rownames(tmp.f),fvalue=c(tmp.f))
        residual <- data.frame(date=dates[i+1],stockname=tmp.tsfr$stockID,res=tmp.residual)
      }else{
        f <- rbind(f,data.frame(date=dates[i+1],fname=rownames(tmp.f),fvalue=c(tmp.f)))
        residual <- rbind(residual,data.frame(date=dates[i+1],stockname=tmp.tsfr$stockID,res=tmp.residual))
      }
    }
    
  }else{
    dates <- unique(TSFR$date)
    for(i in 1:(length(dates)-1)){
      tmp.tsfr <- TSFR[TSFR$date==dates[i],]
      tmp.x <- as.matrix(tmp.tsfr[,-c(1,2,ncol(tmp.tsfr)-1,ncol(tmp.tsfr))])
      tmp.x <- subset(tmp.x,select = (colnames(tmp.x)[colSums(tmp.x)!=0]))
      tmp.r <- as.matrix(tmp.tsfr[,"periodrtn"])
      tmp.r[is.na(tmp.r)] <- mean(tmp.r,na.rm = T)
      tmp.f <- solve(crossprod(tmp.x)) %*% t(tmp.x) %*% tmp.r
      tmp.residual <- tmp.r-tmp.x %*% tmp.f
      if(i==1){
        f=data.frame(date=dates[i+1],fname=rownames(tmp.f),fvalue=c(tmp.f))
        residual <- data.frame(date=dates[i+1],stockname=tmp.tsfr$stockID,res=tmp.residual)
      }else{
        f <- rbind(f,data.frame(date=dates[i+1],fname=rownames(tmp.f),fvalue=c(tmp.f)))
        residual <- rbind(residual,data.frame(date=dates[i+1],stockname=tmp.tsfr$stockID,res=tmp.residual))
      }
    }
  }
  alphafname <- sapply(alphafactorLists,'[[','factorName')
  alphaf <- f[f$fname %in% alphafname,]
  riskf <- f[!(f$fname %in% alphafname),]
  re <- list(TSFR,alphaf,riskf,residual)
  tpassed <- proc.time()-ptm
  tpassed <- tpassed[3]
  cat("This function running time is ",tpassed/60,"min.")
  return(re)
}



#' calcFDelta
#'
#' calculate F covariance matrix and Delta
#' @author Andrew Dow
#' @param riskf is a factor return dataframe.
#' @param residual is a residual dataframe.
#' @return a list,contains Fcov and Delta.
#' @examples 
#' calcFDelta(riskf,residual)
calcFDelta <- function(riskf,residual,rollingperiod=36){
  ptm <- proc.time()
  f <- dcast(riskf,date~fname,fill=0,value.var = 'fvalue')
  f <- arrange(f,date)
  # the follow rolling covariance for loop can be optimized.
  for(i in rollingperiod:nrow(f)){
    tmp.f <- f[(i-rollingperiod+1):i,]
    tmp.Fcov <- cov(tmp.f[,-1])
    
    tmp.residual <- residual[residual$date %in% tmp.f$date,]
    tmp.Delta <- ddply(tmp.residual,.(stockname),summarize,var=var(res))
    tmp.Delta <- cbind(date=f$date[i],tmp.Delta)
    if(i==rollingperiod){
      Fcov <- data.frame(date=f$date[i],tmp.Fcov)
      Delta <- tmp.Delta
    } 
    else{
      Fcov <- rbind(Fcov,data.frame(date=f$date[i],tmp.Fcov))
      Delta <- rbind(Delta,tmp.Delta)
    } 
  }
  
  re <- list(Fcov,Delta)
  tpassed <- proc.time()-ptm
  tpassed <- tpassed[3]
  cat("This function running time is ",tpassed,"s.")
  return(re)
  
}


#' calcAlphaf
#'
#' calculate alpha factors' factor return
#' @author Andrew Dow
#' @param f is a factor return dataframe.
#' @param residual is a residual dataframe.
#' @return alpha factors' factor return data.
#' @examples 
#' calcAlphaf(alphaf,meanperiod=12)
calcAlphaf <- function(alphaf,meanperiod=12){
  f <- arrange(alphaf,fname,date)
  f <- ddply(f, "fname",transform,fmean = rollmean(fvalue, 12, na.pad=TRUE,align='right'))
  f <- subset(f,!is.na(fmean),select=c(date,fname,fmean))
  f <- arrange(f,date,fname)
  return(f)
}


#' OptWgt
#'
#' optimize portfolio weight.
#' @author Andrew Dow
#' @param alphaf is alpha factors' factor return.
#' @param riskf is risk factors' factor return.
#' @param covmat is the covariance matrix.
#' @param control is optimization constraint,\bold{IndSty} means industry and style neutral,
#' \bold{Ind} means only industry neutral,
#' \bold{IndStyTE} means besides industry and style neutral,tracking error also required.
#' @param benchmark is the benckmark for optimization.
#' @return .
#' @examples 
#' 
OptWgt <- function(alphaf,riskf,covmat,control=c('IndSty','Ind','IndStyTE'),benchmark='EI000905'){
  ptm <- proc.time()
  control <- match.arg(control) 
  
  
  
  tpassed <- proc.time()-ptm
  tpassed <- tpassed[3]
  cat("This function running time is ",tpassed,"s.")
}


RebDates <- getRebDates(as.Date('2009-12-31'),as.Date('2015-12-31'),'month')
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


data <- calcfres(TS,alphafactorLists,riskfactorLists,regresstype = 'glm')
TSFR <- data[[1]]
alphaf <- data[[2]]
riskf <- data[[3]]
residual <- data[[4]]

data <- calcFDelta(riskf,residual)
Fcov <- data[[1]]
Delta <- data[[2]]

alphafmean <- calcAlphaf(alphaf)






