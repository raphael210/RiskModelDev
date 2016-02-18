#load packages
suppressMessages(library(RFactorModel))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))
suppressMessages(library(plyr))
suppressMessages(library(stringr))


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




#' calcf
#'
#' calculate lower case f
#' @author Andrew Dow
#' @param TSR is a TSR object.
#' @param riskfactors is a set of risky factor for regressing.
#' @param regresstype choose the regress type,the default type is glm
#' @return a TSF object
#' @examples 
#' factorLists <- buildFactorLists(
#' buildFactorList(factorFun = "gf.liquidity",factorDir = -1,factorNA = "median",factorStd = "norm"),
#' buildFactorList(factorFun = "gf.ln_mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm"))
#' calcf(TSR,riskfactors=factorLists)

calcf <- function(TSR,riskfactors=factorLists,regresstype=c('glm','lm')){
  regresstype <- match.arg(regresstype)
  
}


RebDates <- getRebDates(as.Date('2010-01-31'),as.Date('2015-11-30'),'month')
TS <- getTS(RebDates,'EI000985')
TSR <- getTSR(TS)

  
TSF <- getTSF(TS, gf.liquidity, factorDir = -1,
                          factorOutlier = 3, factorStd ="norm",
                          factorNA = "median",
                          sectorAttr = defaultSectorAttr())
TSFR <- getTSR(TSF)
TSFR <- rename(TSFR,c("factorscore"="liquidity"))
TSFR[is.na(TSFR$periodrtn),"periodrtn"] <- 0
#get total market value
mvf <- gf.mkt_cap(TS)
mvf[is.na(mvf$factorscore),"factorscore"] <- median(mvf$factorscore,na.rm = T)
mvf$factorscore <- log(mvf$factorscore)
mvf$factorscore <- (mvf$factorscore-mean(mvf$factorscore))/sd(mvf$factorscore)
mvf <- rename(mvf,c("factorscore"="marketvalue"))

#get liquid market value
fvf <- gf.float_cap(TS)
fvf[is.na(fvf$factorscore),"factorscore"] <- median(fvf$factorscore,na.rm = T)
fvf$factorscore <- fvf$factorscore/10000
fvf <- rename(fvf,c("factorscore"="liquidvalue"))

#get sector id
TSS <- getSectorID(TS)
TSS[is.na(TSS$sector),"sector"] <- "ESNone"
TSS <- dcast(TSS,date+stockID~sector,length,fill=0)

TSFR <- merge(TSFR,mvf,by =c("date","stockID"))
TSFR <- merge(TSFR,fvf,by =c("date","stockID"))
TSFR <- merge(TSFR,TSS,by =c("date","stockID"))

dates <- unique(TSFR$date)
for(i in dates){
  
  tmp.tsfr <- TSFR[TSFR$date==i,]
  tmp.x <- as.matrix(tmp.tsfr[,-c(1,2,4,5,7)])
  tmp.w <- diag(tmp.tsfr[,"liquidvalue"])
  tmp.r <- as.matrix(tmp.tsfr[,"periodrtn"])
  tmp.f <- solve(crossprod(tmp.x,tmp.w) %*% tmp.x) %*% crossprod(tmp.x,tmp.w) %*% tmp.r
  glm1 <- glm()
}





