#load packages
suppressMessages(library(RFactorModel))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))
suppressMessages(library(plyr))
suppressMessages(library(stringr))
suppressMessages(library(quadprog))
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
  oridata <- oridata[,c('date','wind_code')]
  oridata$wind_code <- str_c('EQ',str_sub(oridata$wind_code,1,6))
  dates <- unique(oridata$date)
  for(i in 1:length(dates)){
    if(i==1){
      tmp <- oridata[oridata$date==dates[i],]
      re <- data.frame(indexID=str_c("EI",str_sub(indexID,1,6)),SecuID=tmp$wind_code,
                       InDate=tmp$date,OutDate=as.Date('1900-01-01'),Flag=1,UpdateTime=tmp$date)
    }else{
      tmp1 <- oridata[oridata$date==dates[i-1],]
      tmp2 <- oridata[oridata$date==dates[i],]
      outstock <- setdiff(tmp1$wind_code,tmp2$wind_code)
      instock <- setdiff(tmp2$wind_code,tmp1$wind_code)
      if(length(outstock)>0){
        re[(re$SecuID %in% outstock)&re$Flag==1,'OutDate'] <- dates[i]
        re[(re$SecuID %in% outstock)&re$Flag==1,'UpdateTime'] <- dates[i]
        re[(re$SecuID %in% outstock)&re$Flag==1,'Flag'] <- 0
      }
      if(length(instock)>0){
        tmp2 <- tmp2[tmp2$wind_code %in% instock,]
        tmp.re <- data.frame(indexID=str_c("EI",str_sub(indexID,1,6)),SecuID=tmp2$wind_code,
                             InDate=tmp2$date,OutDate=as.Date('1900-01-01'),Flag=1,UpdateTime=tmp2$date)
        re <- rbind(re,tmp.re)
      }
    }
  }
  re[re$OutDate=='1900-01-01',"OutDate"] <- NA
  re <- transform(re,InDate=rdate2int(InDate),OutDate=rdate2int(OutDate))
  dbWriteTable(db.local(),"LC_IndexComponent",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
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


#'fix.lcdb.swindustry
#'
#' fix local database's shenwan industry rule's bug and make the rule keep consistent.
#' @author Andrew Dow
#' @return nothing.
#' @examples 
#' fix.lcdb.swindustry()
fix.lcdb.swindustry <- function(){
  qr <- "select * from LC_ExgIndustry where Standard=33"
  re <- dbGetQuery(db.local(),qr)
  if(nrow(re)>0) return("Already in local database!")
  
  qr <- "SELECT s.SecuCode 'stockID',l.CompanyCode,l.FirstIndustryCode 'Code1',l.FirstIndustryName 'Name1',
  l.SecondIndustryCode 'Code2',l.SecondIndustryName 'Name2',l.ThirdIndustryCode 'Code3',
  l.ThirdIndustryName 'Name3',convert(varchar, l.InfoPublDate, 112) 'InDate',
  convert(varchar, l.CancelDate, 112) 'OutDate',l.InfoSource,l.Standard,l.Industry,
  l.IfPerformed 'Flag',l.XGRQ 'UpdateTime'
  FROM [JYDB].[dbo].[LC_ExgIndustry] l,JYDB.dbo.SecuMain s
  where l.CompanyCode=s.CompanyCode and s.SecuCategory=1 and l.Standard in(9,24)"
  re <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
  re <- re[str_sub(re$stockID,1,2) %in% c('60','30','00'),]
  re <- re[ifelse(is.na(re$OutDate),T,re$OutDate!=re$InDate),]
  re$stockID <- str_c("EQ",re$stockID)
  
  sw24use <- re[(re$InDate>20140101) & (re$Standard==24),]
  sw9use <- re[(re$InDate<20140101) & (re$Standard==9),]
  sw24tmp <- re[(re$InDate==20140101) & (re$Standard==24),]
  sw9tmp <- sw9use[is.na(sw9use$OutDate) | sw9use$OutDate>20140101,]
  sw9tmp <- sw9tmp[,c("stockID","Code1","Name1","Code2","Name2","Code3","Name3")]
  colnames(sw9tmp) <- c("stockID","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")
  hashtable <- merge(sw24tmp,sw9tmp,by='stockID',all.x=T)
  hashtable <- hashtable[,c("Code1","Name1","Code2","Name2","Code3","Name3","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")]
  hashtable <- unique(hashtable)
  tmp <- as.data.frame(table(hashtable$OldName3))
  tmp <- tmp[tmp$Freq==1,]
  hashtable <- hashtable[hashtable$OldName3 %in% tmp$Var1,]
  
  sw9use <- rename(sw9use,replace=c("Code1"="OldCode1",
                          "Name1"="OldName1",
                          "Code2"="OldCode2",
                          "Name2"="OldName2",
                          "Code3"="OldCode3",
                          "Name3"="OldName3"))
  sw9use <- merge(sw9use,hashtable,by=c("OldCode1","OldName1", 
                                        "OldCode2","OldName2",
                                        "OldCode3","OldName3"),all.x=T)
  sw9use <- sw9use[,c("stockID","CompanyCode","Code1","Name1","Code2","Name2",      
                       "Code3","Name3","InDate","OutDate","InfoSource","Standard", 
                       "Industry","Flag","UpdateTime","OldCode1","OldName1","OldCode2",
                      "OldName2","OldCode3","OldName3")]
  tmp <- sw9use[is.na(sw9use$Code1),c("stockID","CompanyCode","InDate","OutDate","InfoSource","Standard", 
                                      "Industry","Flag","UpdateTime","OldCode1","OldName1","OldCode2",
                                      "OldName2","OldCode3","OldName3")]
  
  tmp <- arrange(tmp,stockID,InDate)
  tmp2 <- sw24tmp[,c("stockID","Code1","Name1","Code2","Name2","Code3","Name3")]
  tmp <-merge(tmp,tmp2,by='stockID',all.x=T) 
  tmp[is.na(tmp$Code1),c("Name1","Name2","Name3")] <- '综合'
  tmp[is.na(tmp$Code1),"Code1"] <-510000
  tmp[is.na(tmp$Code2),"Code3"] <-510100
  tmp[is.na(tmp$Code3),"Code3"] <-510101
  tmp <- tmp[,c("stockID","CompanyCode","Code1","Name1","Code2","Name2",      
                "Code3","Name3","InDate","OutDate","InfoSource","Standard", 
                "Industry","Flag","UpdateTime")]
  
  sw9use <- sw9use[!is.na(sw9use$Code1),c("stockID","CompanyCode","Code1","Name1","Code2","Name2",      
                                                      "Code3","Name3","InDate","OutDate","InfoSource","Standard", 
                                                      "Industry","Flag","UpdateTime")]
  sw9use <- rbind(sw9use,tmp)

  sw33 <- rbind(sw9use,sw24use)
  sw33$Standard <- 33
  sw33$Code1 <- str_c('ES33',sw33$Code1)
  sw33$Code2 <- str_c('ES33',sw33$Code2)
  sw33$Code3 <- str_c('ES33',sw33$Code3)
  sw33$Code99 <- c(NA)
  sw33$Name99 <- c(NA)
  sw33$Code98 <- c(NA)
  sw33$Name98 <- c(NA)
  sw33 <- arrange(sw33,stockID,InDate)
  
  #deal with abnormal condition
  #1 outdate<indate
  sw33 <- sw33[ifelse(is.na(sw33$OutDate),T,sw33$OutDate>=sw33$InDate),]
  #2 one stock has two null outdate
  tmp <- ddply(sw33,.(stockID),summarise,NANum=sum(is.na(OutDate)))
  tmp <- c(tmp[tmp$NANum>1,'stockID'])
  sw33tmp <- sw33[sw33$stockID %in% tmp,]
  sw33 <- sw33[!(sw33$stockID %in% tmp),]
  if(nrow(sw33tmp)>0){
    for(i in 1:(nrow(sw33tmp)-1)){
      if(sw33tmp$stockID[i]==sw33tmp$stockID[i+1] && is.na(sw33tmp$OutDate[i])) sw33tmp$OutDate[i] <- sw33tmp$InDate[i+1]
    }
  }
  sw33 <- rbind(sw33,sw33tmp)
  sw33 <- arrange(sw33,stockID,InDate)
  #3 indate[i+1]!=outdate[i]
  sw33$tmpstockID <- c(NA,sw33$stockID[1:(nrow(sw33)-1)])
  sw33$tmpOutDate <- c(NA,sw33$OutDate[1:(nrow(sw33)-1)])
  sw33$InDate <- ifelse(ifelse(is.na(sw33$tmpstockID) | is.na(sw33$tmpOutDate),FALSE,sw33$stockID==sw33$tmpstockID & sw33$InDate!=sw33$tmpOutDate),
                        sw33$tmpOutDate,sw33$InDate)
  sw33 <- subset(sw33,select=-c(tmpstockID,tmpOutDate))
  
  dbWriteTable(db.local(),'LC_ExgIndustry',sw33,overwrite=FALSE,append=TRUE,row.names=FALSE)
  return('Done!')
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


#' getTSFQuick
#'
#' get TSF data quickly
#' @author Andrew Dow
#' @param TS is a TS object.
#' @param alphafactorLists is a set of alpha factors for regressing.
#' @param riskfactorLists is a set of risk factors for regressing.
#' @return a TSF object.
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
#'  TSF <- getTSFQuick(TS,alphafactorLists,riskfactorLists)
getTSFQuick <- function(TS,alphafactorLists,riskfactorLists){
  ptm <- proc.time()
  load("~/R/FactorModelDev/TSFlocal.RData")
  
  factorLists <- c(alphafactorLists,riskfactorLists)
  #deal with new factor
  fname <- sapply(factorLists,'[[','factorName')
  oldfname <- unique(TSFlocal$FactorName)
  newfname <- setdiff(fname,oldfname)
  if(length(newfname)>0){
    cond <- sapply(factorLists, function(x)  x$factorName %in% newfname)
    factorLists2 <- factorLists[cond]
    TS2 <- unique(TSFlocal[,c('date','stockID')])
    wgts <- rep(1/length(factorLists2),length(factorLists2))
    TSF2 <- getMultiFactor(TS2,factorLists2,wgts)
    TSF2 <- subset(TSF2,select = -factorscore)
    if("NP_YOY" %in% colnames(TSF2)) TSF2 <- subset(TSF2,select = -c(InfoPublDate,EndDate,src,NP_LYCP))
    TSFlocal2 <- melt(TSF2,id=c('date','stockID'),variable.name = "FactorName")
    TSFlocal <- rbind(TSFlocal,TSFlocal2)
    save(TSFlocal,file='TSFlocal.RData')
  }
  
  #deal with new date
  olddate <- unique(TSFlocal$date)
  dates <- unique(TS$date)
  newdate <- as.Date(setdiff(dates,olddate))
  if(length(newdate)>0){
    TS3 <- getTS(newdate,'EI000985')
    wgts <- rep(1/length(factorLists),length(factorLists))
    TSF3 <- getMultiFactor(TS3,factorLists,wgts)
    TSF3 <- subset(TSF3,select = -factorscore)
    if("NP_YOY" %in% colnames(TSF3)) TSF3 <- subset(TSF3,select = -c(InfoPublDate,EndDate,src,NP_LYCP))
    TSFlocal3 <- melt(TSF3,id=c('date','stockID'),variable.name = "FactorName")
    TSFlocal <- rbind(TSFlocal,TSFlocal3)
    save(TSFlocal,file='TSFlocal.RData')
  }
  
  TSFuse <- TSFlocal[TSFlocal$FactorName %in%fname,]
  TSF <- merge(TS,TSFuse,by=c('date','stockID'),all.x=T)
  TSF <- TSF[!is.na(TSF$FactorName),]
  TSF <- dcast(TSF,date+stockID~FactorName)
  
  tpassed <- proc.time()-ptm
  tpassed <- tpassed[3]
  cat("loading data costs ",tpassed/60,"min.\n")
  return(TSF)
}



#' calcfres
#'
#' calculate factor return f data and the residual data
#' @author Andrew Dow
#' @param TSF is a TSF object.
#' @param alphafactorLists is a set of alpha factors for regressing.
#' @param riskfactorLists is a set of risk factors for regressing.
#' @param regresstype choose the regress type,the default type is glm.
#' @return a list,contains TSFR,alphaf,riskf,residual
#' @examples 
#' calcfres(TSF,alphafactorLists,riskfactorLists)
calcfres <- function(TSF,alphafactorLists,riskfactorLists,regresstype=c('glm','lm')){
  ptm <- proc.time()
  regresstype <- match.arg(regresstype)
  
  TS <- TSF[,c('date','stockID')]
  
  cat("getting sector id......","\n")
  TSS <- getSectorID(TS,sectorAttr = list(33,1))
  TSS[is.na(TSS$sector),'sector'] <- 'ES33510000'
  TSS <- dcast(TSS,date+stockID~sector,length,fill=0,value.var = 'sector')
  
  cat("getting period return......","\n")
  TSR <- getTSR(TS)
  
  #merge data
  TSF <- merge(TSF,TSS,by =c("date","stockID"))
  TSFR <- merge(TSF,TSR,by =c("date","stockID"))
  
  cat("calculating f and residual......","\n")
  if(regresstype=='glm'){
    #get liquid market value
    TSFv <- getTSF(TS,'gf.float_cap',factorStd = 'none',factorNA = "median")
    TSFv$factorscore <- sqrt(TSFv$factorscore)
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
        residual <- data.frame(date=dates[i+1],stockID=tmp.tsfr$stockID,res=tmp.residual)
      }else{
        f <- rbind(f,data.frame(date=dates[i+1],fname=rownames(tmp.f),fvalue=c(tmp.f)))
        residual <- rbind(residual,data.frame(date=dates[i+1],stockID=tmp.tsfr$stockID,res=tmp.residual))
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
        residual <- data.frame(date=dates[i+1],stockID=tmp.tsfr$stockID,res=tmp.residual)
      }else{
        f <- rbind(f,data.frame(date=dates[i+1],fname=rownames(tmp.f),fvalue=c(tmp.f)))
        residual <- rbind(residual,data.frame(date=dates[i+1],stockID=tmp.tsfr$stockID,res=tmp.residual))
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
    tmp.Delta <- ddply(tmp.residual,.(stockID),summarize,var=var(res))
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
#' @param Fcov is the covariance matrix.
#' @param constr is optimization constraint,\bold{IndSty} means industry and style neutral,
#' \bold{IndStyTE} means besides industry and style neutral,tracking error also required.
#' @param benchmark is the benckmark for optimization.
#' @return .
#' @examples 
#' 
OptWgt <- function(TSF,alphaf,Fcov,Delta,constr=c('IndSty','Ind','IndStyTE'),benchmark='EI000905',riskavr=1,indfexp=0.05,...){
  ptm <- proc.time()
  constr <- match.arg(constr) 
  alphafname <- unique(as.character(alphaf$fname))
  tmp <- setdiff(colnames(TSF[,-c(1,2)]),alphafname)
  indfname <- tmp[str_detect(tmp,'ES')]
  riskfname <- setdiff(tmp,indfname)
  dates <- unique(TSF$date)
  if(constr=='Ind'){
    for(i in dates){
      cat('finishing ',round(match(i,dates)/length(dates)*100),'%.\n') 
      #get one period raw data
      tmp.TSF <- TSF[TSF$date==i,]
      tmp.alphaf <- alphaf[alphaf$date==i,]
      rownames(tmp.alphaf) <- tmp.alphaf$fname
      tmp.alphaf <- tmp.alphaf[alphafname,]
      tmp.Fcov <- Fcov[Fcov$date==i,-1]
      rownames(tmp.Fcov) <- colnames(tmp.Fcov)
      tmp.Fcov <- tmp.Fcov[indfname,indfname]
      tmp.Delta <- Delta[Delta$date==i,]
      
      #get benchmark stock component weight and sector info. 
      benchmarkdata <- getIndexCompWgt(indexID = benchmark,i)
      sec <- getSectorID(benchmarkdata[,c('date','stockID')],sectorAttr = list(33,1))
      sec[is.na(sec$sector),"sector"] <- 'ES33510000'
      benchmarkdata <- merge(benchmarkdata,sec,by=c('date','stockID'))
      totwgt <- ddply(benchmarkdata,.(sector),summarise,secwgt=sum(wgt))
      totwgt$wgtlb <- totwgt$secwgt*(1-indfexp)
      totwgt$wgtub <- totwgt$secwgt*(1+indfexp)
      rownames(totwgt) <- totwgt$sector
      
      
      #deal with missing industry
      missind <- setdiff(indfname,totwgt$sector)
      if(length(missind)>0){
        indfname <- setdiff(indfname,missind)
        tmp.Fcov <- tmp.Fcov[indfname,indfname]
        tmp.TSF <- tmp.TSF[tmp.TSF[,missind]==0,]
        tmp.TSF <- tmp.TSF[,!colnames(tmp.TSF)%in% missind]
      }
      totwgt <- totwgt[indfname,]
      
      
      #prepare matrix data
      alphamat <- as.matrix(tmp.TSF[,alphafname])
      riskmat <- as.matrix(tmp.TSF[,indfname])
      rownames(alphamat) <- tmp.TSF$stockID
      rownames(riskmat) <- tmp.TSF$stockID
      
      ret <- t(as.matrix(tmp.alphaf$fmean))%*% t(alphamat)
      ret <- ret*riskavr
      Fcovmat <- as.matrix(tmp.Fcov)
      Deltamat <- data.frame(stockID=rownames(alphamat))
      Deltamat <- merge(Deltamat,tmp.Delta[,-1],by='stockID',all.x = T)
      Deltamat[is.na(Deltamat$var),"var"] <- median(Deltamat$var,na.rm = T)
      Deltamat <- diag(c(Deltamat$var))
      Dmat <- riskmat%*%Fcovmat%*%t(riskmat)+Deltamat
      Amat <- cbind(riskmat,-1*riskmat)
      nstock <- dim(Dmat)[1]
      Amat <- cbind(1,Amat,diag(x=1,nstock),diag(x=-1,nstock))#control weight
      bvec <- c(1,totwgt$wgtlb,-1*totwgt$wgtub,rep(0,nstock),rep(-0.01,nstock))
      system.time(res <- solve.QP(Dmat,ret,Amat,bvec,meq = 1))

      res$solution[abs(res$solution) <= 1e-3] <- 0
      tmp <- data.frame(date=as.Date(i),stockID=rownames(alphamat),wgt=res$solution)
      tmp <- merge(tmp,benchmarkdata[,c("date","stockID","wgt")],by = c("date","stockID"),all.x = T)
      colnames(tmp) <-c( "date","stockID","wgtopt","wgtinbench")
      tmp.sec <- getSectorID(tmp[,c('date','stockID')],sectorAttr = list(33,1))
      tmp.sec[is.na(tmp.sec$sector),"sector"] <- 'ES33510000'
      tmp <- merge(tmp,tmp.sec,by=c('date','stockID'))
      if(i==dates[1]){
        result <- tmp
      }else{
        result <- rbind(result,tmp)
      }
      
    }
  }else if(constr=='IndSty'){
    allriskfname <- c(riskfname,indfname)
    riskfexp <- list(...)
    for(i in dates){
        cat('finishing ',round(match(i,dates)/length(dates)*100),'%.\n')    
        tmp.TSF <- TSF[TSF$date==i,]
        tmp.alphaf <- alphaf[alphaf$date==i,]
        rownames(tmp.alphaf) <- tmp.alphaf$fname
        tmp.alphaf <- tmp.alphaf[alphafname,]
        tmp.Fcov <- Fcov[Fcov$date==i,-1]
        rownames(tmp.Fcov) <- colnames(tmp.Fcov)
        tmp.Fcov <- tmp.Fcov[allriskfname,allriskfname]
        tmp.Delta <- Delta[Delta$date==i,]
        
        #get benchmark stock component weight and sector info. 
        benchmarkdata <- getIndexCompWgt(indexID = benchmark,i)
        sec <- getSectorID(benchmarkdata[,c('date','stockID')],sectorAttr = list(33,1))
        sec[is.na(sec$sector),"sector"] <- 'ES33510000'
        benchmarkdata <- merge(benchmarkdata,sec,by=c('date','stockID'))
      
        secwgt <- ddply(benchmarkdata,.(sector),summarise,secwgt=sum(wgt))
        secwgt$wgtlb <- secwgt$secwgt*(1-indfexp)
        secwgt$wgtub <- secwgt$secwgt*(1+indfexp)
        
        #deal with missing industry
        missind <- setdiff(indfname,secwgt$sector)
        if(length(missind)>0){
          allriskfname <- setdiff(allriskfname,missind)
          tmp.Fcov <- tmp.Fcov[allriskfname,allriskfname]
          tmp.TSF <- tmp.TSF[tmp.TSF[,missind]==0,]
          tmp.TSF <- tmp.TSF[,!colnames(tmp.TSF)%in% missind]
        }

        #get benchmark risk factor value
        benchmarkdata <- merge(benchmarkdata,tmp.TSF[,c('date','stockID',riskfname)],by=c('date','stockID'))
        riskfwgt <- t(as.matrix(benchmarkdata$wgt))%*%as.matrix(benchmarkdata[,riskfname])
        riskfwgt <- data.frame(sector=colnames(riskfwgt),secwgt=c(riskfwgt))
        riskfwgt <- merge(riskfwgt,riskfexp,by='sector')
        riskfwgt$wgtlb <- ifelse(riskfwgt$secwgt>0,riskfwgt$secwgt*(1+riskfwgt$lb),riskfwgt$secwgt*(1-riskfwgt$lb))
        riskfwgt$wgtub <- ifelse(riskfwgt$secwgt>0,riskfwgt$secwgt*(1+riskfwgt$ub),riskfwgt$secwgt*(1-riskfwgt$ub))
        riskfwgt <- riskfwgt[,c("sector","secwgt","wgtlb","wgtub")]
        totwgt <- rbind(riskfwgt,secwgt)
        rownames(totwgt) <- totwgt$sector
        totwgt <- totwgt[allriskfname,]
        
        #prepare matrix data
        alphamat <- as.matrix(tmp.TSF[,alphafname])
        riskmat <- as.matrix(tmp.TSF[,allriskfname])
        rownames(alphamat) <- tmp.TSF$stockID
        rownames(riskmat) <- tmp.TSF$stockID
        
        ret <- t(as.matrix(tmp.alphaf$fmean))%*% t(alphamat)
        ret <- ret*riskavr
        Fcovmat <- as.matrix(tmp.Fcov)
        Deltamat <- data.frame(stockID=rownames(alphamat))
        Deltamat <- merge(Deltamat,tmp.Delta[,-1],by='stockID',all.x = T)
        Deltamat[is.na(Deltamat$var),"var"] <- median(Deltamat$var,na.rm = T)
        Deltamat <- diag(c(Deltamat$var))
        Dmat <- riskmat%*%Fcovmat%*%t(riskmat)+Deltamat
        Amat <- cbind(riskmat,-1*riskmat)
        nstock <- dim(Dmat)[1]
        Amat <- cbind(1,Amat,diag(x=1,nstock),diag(x=-1,nstock))#control weight
        bvec <- c(1,totwgt$wgtlb,-1*totwgt$wgtub,rep(0,nstock),rep(-0.01,nstock))
        system.time(res <- solve.QP(Dmat,ret,Amat,bvec,meq=1))
        
        res$solution[abs(res$solution) <= 1e-3] <- 0
        tmp <- data.frame(date=as.Date(i),stockID=rownames(alphamat),wgt=res$solution)
        tmp <- merge(tmp,benchmarkdata[,c("date","stockID","wgt")],by = c("date","stockID"),all.x = T)
        colnames(tmp) <-c( "date","stockID","wgtopt","wgtinbench")
        tmp.sec <- getSectorID(tmp[,c('date','stockID')],sectorAttr = list(33,1))
        tmp.sec[is.na(tmp.sec$sector),"sector"] <- 'ES33510000'
        tmp <- merge(tmp,tmp.sec,by=c('date','stockID'))
        if(i==dates[1]){
          result <- tmp
        }else{
          result <- rbind(result,tmp)
        }
        
      }

  }else{
    
  }

  
  tpassed <- proc.time()-ptm
  tpassed <- tpassed[3]
  cat("This function running time is ",tpassed,"s.")
}


