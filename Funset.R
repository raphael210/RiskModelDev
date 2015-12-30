
# this function is designed to add a new index to local database
add.index <- function(indexID="000985"){
  #part 1 update local SecuMain
  qr <- "select * from dbo.SecuMain
  WHERE SecuCode='000985'
  and SecuCategory=4"
  re <- sqlQuery(db.jy(),qr)
  tmp <- dbGetQuery(db.local(),"select * from SecuMain where ID='EI000300'")
  re <- transform(re,ID='EI000985',SecuCode=str_pad(SecuCode,6,pad='0'),
                  StockID_TS='SH000985',StockID_wind='000985.SH',
                  ListedDate=rdate2int(ListedDate),UpdateTime=JSID)
  re <- re[,colnames(tmp)]
  dbWriteTable(db.local(),"SecuMain",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
  
  #part 2 update local LC_IndexComponent
  library(timeDate)
  library(WindR)
  w.start()
  dates <- seq.Date(as.Date('2004-12-01'),Sys.Date(),by="6 month")
  dates <- as.Date(timeNthNdayInMonth(dates, nday = 5, nth = 2, format = "%Y-%m-%d"))
  dates <- trday.nearby(dates,-1)
  
  subfun <- function(date){
    tmp<-w.wset('SectorConstituent',date=date,windcode='000985.SH')$Data
    return(tmp)
  }
  result<-ldply(dates,subfun)
  result <- ddply(result, "wind_code", summarise,indate=min(date),outdate=max(date))
  dates <- data.frame(outdate=dates,new=c(dates[2:length(dates)],NA))
  result <- merge(result,dates,by = 'outdate')
  result <- transform(result,indexID=c('EI000985'),Flag=ifelse(is.na(new),1,0),UpdateTime=Sys.time())
  result <- result[,c("indexID","wind_code","indate","new","Flag","UpdateTime")]
  colnames(result) <- c("indexID","SecuID","InDate","OutDate","Flag","UpdateTime")
  result <- transform(result,SecuID=str_sub(str_c('EQ',SecuID,sep = ''),start = 1,end = 8),
                      InDate=rdate2int(InDate),OutDate=rdate2int(OutDate))
  dbWriteTable(db.local(),"LC_IndexComponent",result,overwrite=FALSE,append=TRUE,row.names=FALSE)
}


creat_liquid_factor <- function(type=c("update","new")){
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
  # -- filtering
  return(re)
  
}

RebDates <- getRebDates(as.Date('2012-01-31'),as.Date('2014-12-31'),'month')
TS <- getTS(RebDates,'EI000985')
system.time(TSF <- getTSF(TS, gf.float_cap, factorDir = 1,
       factorOutlier = 3, factorStd ="norm",
       factorNA = "median",
       sectorAttr = defaultSectorAttr()))
system.time(TSFR <-  getTSR(TSF))







