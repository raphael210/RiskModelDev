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


build.liquid.factor <- function(type=c("new","update")){
  time <- Sys.time()
  type <- match.arg(type)
  if(type=='new'){
    qr <- "select t.ID,t.TradingDay,t.TurnoverVolume,t.NonRestrictedShares 
    from QT_DailyQuote t
    where t.TradingDay>=20040101"
    re <- dbGetQuery(db.local(),qr)
    
    re$TurnoverRate <- re$TurnoverVolume/(re$NonRestrictedShares*10000)
    re <- re[,c("ID","TradingDay","TurnoverRate")]
    re <- arrange(re,ID,TradingDay)
    
    tmp <- as.data.frame(table(re$ID))
    tmp <- tmp[tmp$Freq>=21,]
    tmp <- tmp[substr(tmp$Var1, 1,3) %in% c('EQ0',"EQ6","EQ3"),]
    re <- re[re$ID %in% tmp$Var1,]
    
    result <- ddply(re,"ID",mutate,STOM=rollsum(TurnoverRate,21,fill=NA,align = 'right'))
    result <- subset(result,!is.na(result$STOM))
    result$STOM <- log(result$STOM)
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
    
    tmp <- as.data.frame(table(re$ID))
    tmp <- tmp[tmp$Freq>=21,]
    tmp <- tmp[substr(tmp$Var1, 1,3) %in% c('EQ0',"EQ6","EQ3"),]
    re <- re[re$ID %in% tmp$Var1,]
    
    result <- ddply(re,"ID",mutate,STOM=rollsum(TurnoverRate,21,fill=NA,align = 'right'))
    result <- subset(result,!is.na(result$STOM))
    result$STOM <- log(result$STOM)
    result <- subset(result,TradingDay>date)
    result <- melt(result,id=c("ID","TradingDay"))
    colnames(result) <- c("ID","TradingDay","FactorName","FactorScore")
    dbWriteTable(db.local(),"QT_FactorScore_Liquidity",result,overwrite=FALSE,append=TRUE,row.names=FALSE)
  }
  print(Sys.time()-time)
}

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

RebDates <- getRebDates(as.Date('2012-01-31'),as.Date('2014-12-31'),'month')
TS <- getTS(RebDates,'EI000985')
system.time(TSF <- getTSF(TS, gf.liquidity, factorDir = 1,
                          factorOutlier = 3, factorStd ="norm",
                          factorNA = "median",
                          sectorAttr = defaultSectorAttr()))
system.time(TSFR <-  getTSR(TSF))







