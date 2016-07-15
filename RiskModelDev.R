#load packages
suppressMessages(library(RFactorModel))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))
suppressMessages(library(plyr))
suppressMessages(library(stringr))
suppressMessages(library(quadprog))
suppressMessages(library(nloptr))
suppressMessages(library(RODBC))
suppressMessages(library(RSQLite))
suppressMessages(library(ggplot2))
suppressMessages(library(xts))

tsInclude()
tsConnect()

# This function is used to fix bugs in table LC_IndexComponentsWeight in local db.
fix.lcdb.indexcomperr <- function(){
  qr <- "delete from LC_IndexComponentsWeight
  where IndexID in ('EI000905','EI000906') and EndDate=20150930"
  dbSendQuery(db.local(), qr)
  qr <- "SELECT 'EI'+s1.SecuCode 'IndexID','EQ'+s2.SecuCode 'SecuID',
  convert(VARCHAR(9),l.[EndDate],112) 'EndDate',l.[Weight],l.[UpdateTime]
  FROM [JYDB].[dbo].[LC_IndexComponentsWeight] l
  LEFT join JYDB.dbo.SecuMain s1 on l.IndexCode=s1.InnerCode
  LEFT join JYDB.dbo.SecuMain s2 on l.InnerCode=s2.InnerCode
  where s1.SecuCode in('000905','000906') and l.EndDate='2015-09-30'
  order by s1.SecuCode,s2.SecuCode"
  re <- sqlQuery(db.jy(),qr)
  dbWriteTable(db.local(),"LC_IndexComponentsWeight",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
  return('Done')
}


#' add.index.lcdb
#'
#' Add a index to local database
#' @author Andrew Dow
#' @param indexID is the indexID
#' @return nothing
#' @examples 
#' add.index.lcdb(indexID="EI801003")
#' add.index.lcdb(indexID="EI000985")
add.index.lcdb <- function(indexID="EI801003"){
  #check whether the index in local db
  if(indexID=='EI000985'){
    qr <- paste("select * from SecuMain where ID="
                ,str_c("'EI",str_sub(indexID,3,8),"'"),sep="")
    re <- dbGetQuery(db.local(),qr)
    if(nrow(re)>0) return("Already in local database!")
    
    #part 1 update local SecuMain
    qr <- paste("select ID,InnerCode,CompanyCode,SecuCode,SecuAbbr,SecuMarket,  
                ListedSector,ListedState,JSID 'UpdateTime',SecuCode 'StockID_TS',
                SecuCategory,ListedDate,SecuCode 'StockID_wind'
                from SecuMain WHERE SecuCode=",
                str_c("'",str_sub(indexID,start = 3,end = 8),"'"),
                " and SecuCategory=4")
    re <- sqlQuery(db.jy(),qr)
    re <- transform(re,ID=indexID,
                    SecuCode='000985',
                    StockID_TS='SH000985',
                    StockID_wind='000985.SH')
    dbWriteTable(db.local(),"SecuMain",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
    
    #part 2 update local LC_IndexComponent
    qr <- "SELECT 'EI000985' 'IndexID','EQ'+s2.SecuCode 'SecuID',
          convert(varchar(8),l.[InDate],112) 'InDate',
          convert(varchar(8),l.[OutDate],112) 'OutDate',
          l.[Flag],l.[XGRQ] 'UpdateTime',
          convert(varchar(8),s2.ListedDate,112) 'IPODate'
          FROM jydb.dbo.LC_IndexComponent l
          inner join jydb.dbo.SecuMain s1 on l.IndexInnerCode=s1.InnerCode and s1.SecuCode='801003'
          LEFT join jydb.dbo.SecuMain s2 on l.SecuInnerCode=s2.InnerCode"
    re <- sqlQuery(db.jy(),qr,stringsAsFactors=F) 
    re <- transform(re,
                    InDate=intdate2r(InDate),
                    OutDate=intdate2r(OutDate),
                    IPODate=intdate2r(IPODate))
    re[(re$InDate-re$IPODate)<90,'InDate'] <- trday.offset(re[(re$InDate-re$IPODate)<90,'IPODate'],by = months(3))
    re <- re[is.na(re$OutDate-re$InDate) | (re$OutDate-re$InDate)>30,]
    re <- re[substr(re$SecuID,1,3) %in% c('EQ0','EQ3','EQ6'),]
    re <- re[,c("IndexID","SecuID","InDate","OutDate","Flag","UpdateTime")]
    
    qr <- "select 'EQ'+s.SecuCode 'SecuID',st.SpecialTradeType,
          ct.MS,convert(varchar(8),st.InfoPublDate,112) 'InfoPublDate',
          convert(varchar(8),st.SpecialTradeTime,112) 'SpecialTradeTime'
          from jydb.dbo.LC_SpecialTrade st,jydb.dbo.SecuMain s,jydb.dbo.CT_SystemConst ct
          where st.InnerCode=s.InnerCode and SecuCategory=1
          and st.SpecialTradeType=ct.DM and ct.LB=1185 and st.SpecialTradeType in(1,2,5,6)
          order by s.SecuCode"
    st <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
    st <- st[substr(st$SecuID,1,3) %in% c('EQ0','EQ3','EQ6'),]
    st$InDate <- ifelse(st$SpecialTradeType %in% c(2,6),st$SpecialTradeTime,NA)
    st$OutDate <- ifelse(st$SpecialTradeType %in% c(1,5),st$InfoPublDate,NA)
    st$InDate <- intdate2r(st$InDate)
    st$OutDate <- intdate2r(st$OutDate)
    st <- st[,c("SecuID","InDate","OutDate")]
    
    tmp <- rbind(re[,c("SecuID","InDate","OutDate")],st)
    tmp <- melt(tmp,id=c('SecuID'))
    tmp <- tmp[!(is.na(tmp$value) & tmp$variable=='InDate'),]
    tmp <- unique(tmp)
    tmp[is.na(tmp$value),'value'] <- as.Date('2100-01-01')
    tmp <- arrange(tmp,SecuID,value)
    
    tmp$flag <- c(1)
    for(i in 2: nrow(tmp)){
      if(tmp$SecuID[i]==tmp$SecuID[i-1] && tmp$variable[i]==tmp$variable[i-1] && tmp$variable[i]=='InDate'){
        tmp$flag[i-1] <- 0
      }else if(tmp$SecuID[i]==tmp$SecuID[i-1] && tmp$variable[i]==tmp$variable[i-1] && tmp$variable[i]=='OutDate'){
        tmp$flag[i] <- 0
      }else{
        next
      }
    }
    tmp <- tmp[tmp$flag==1,c("SecuID","variable","value")]
    tmp <- arrange(tmp,SecuID,value)
    tmp1 <- tmp[tmp$variable=='InDate',]
    tmp2 <- tmp[tmp$variable=='OutDate',]
    tmp <- cbind(tmp1[,c("SecuID","value")],tmp2[,"value"])
    colnames(tmp) <- c("SecuID","InDate","OutDate")
    tmp[tmp$OutDate==as.Date('2100-01-01'),'OutDate'] <- NA
    tmp$IndexID <- 'EI000985'
    tmp$Flag <- ifelse(is.na(tmp$OutDate),1,0)
    tmp$UpdateTime <- Sys.time()
    tmp$InDate <- rdate2int(tmp$InDate)
    tmp$OutDate <- rdate2int(tmp$OutDate )
    tmp <- tmp[,c("IndexID","SecuID","InDate","OutDate","Flag","UpdateTime")]
    
    dbWriteTable(db.local(),"LC_IndexComponent",tmp,overwrite=FALSE,append=TRUE,row.names=FALSE)
    
    
    
  }else{
    qr <- paste("select * from SecuMain where ID="
                ,str_c("'EI",str_sub(indexID,3,8),"'"),sep="")
    re <- dbGetQuery(db.local(),qr)
    if(nrow(re)>0) return("Already in local database!")
    
    #part 1 update local SecuMain
    qr <- paste("select ID,InnerCode,CompanyCode,SecuCode,SecuAbbr,SecuMarket,  
                ListedSector,ListedState,JSID 'UpdateTime',SecuCode 'StockID_TS',
                SecuCategory,ListedDate,SecuCode 'StockID_wind'
                from SecuMain WHERE SecuCode=",
                str_c("'",str_sub(indexID,start = 3,end = 8),"'"),
                " and SecuCategory=4",sep='')
    re <- sqlQuery(db.jy(),qr)
    re <- transform(re,ID=indexID)
    dbWriteTable(db.local(),"SecuMain",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
    
    #part 2 update local LC_IndexComponent
    qr <- paste("SELECT 'EI'+s1.SecuCode 'IndexID','EQ'+s2.SecuCode 'SecuID',
                convert(varchar(8),l.[InDate],112) 'InDate',
                convert(varchar(8),l.[OutDate],112) 'OutDate',l.[Flag],l.[XGRQ] 'UpdateTime'  
                FROM [JYDB].[dbo].[LC_IndexComponent] l
                inner join JYDB.dbo.SecuMain s1 on l.IndexInnerCode=s1.InnerCode and s1.SecuCode=",str_c("'",str_sub(indexID,start = 3,end = 8),"'"),
                " LEFT join JYDB.dbo.SecuMain s2 on l.SecuInnerCode=s2.InnerCode")
    re <- sqlQuery(db.jy(),qr)  
    dbWriteTable(db.local(),"LC_IndexComponent",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
  }

  return("Done!")
}


#' CT_FactorLists_amtao
#'
#' build a new table similar to table CT_FactorLists in local db to store factors builded by amtao.
#' @author Andrew Dow
#' @param factor.df is a data frame contains factors' detail info.
#' @return nothing
#' @examples 
#' factor.df <- data.frame(factorID='F000021',      
#' factorName='liquidity',       
#' factorFun="gf.ln_mkt_cap",      
#' factorPar='', 
#' factorDir=-1, 
#' factorType='', 
#' factorDesc='')
#' CT_FactorLists_amtao(factor.df)
CT_FactorLists_amtao <- function(factor.df){
  #check whether table CT_FactorLists_amtao exists
  re <- dbExistsTable(db.local(),'CT_FactorLists_amtao')
  if(re){
    #exist
    dbWriteTable(db.local(),"CT_FactorLists_amtao",factor.df,overwrite=FALSE,append=TRUE,row.names=FALSE)
    
  }else{
    #not exist
    dbWriteTable(db.local(),"CT_FactorLists_amtao",factor.df)
  }
  return('Done!')
}




lcdb.build.QT_FactorScore_amtao <- function(factordf=data.frame(factorID=c('F000021','F000022','F000023'),
                                                                factorName=c('liquidity','beta','IVR'),
                                                                stringsAsFactors=FALSE)){
  for(i in 1:nrow(factordf)){
    if(factordf$factorName[i]=='liquidity'){
      cat('building ',factordf$factorName[i],' factor...\n')
      con <- db.local()
      re <- dbReadTable(con, "QT_DailyQuote", select.cols="ID,TradingDay,TurnoverVolume,NonRestrictedShares")
      re <- re[re$TradingDay>=20050101,]
      re$TurnoverRate <- re$TurnoverVolume/(re$NonRestrictedShares*10000)
      re <- re[,c("ID","TradingDay","TurnoverRate")]
      re$TurnoverRate <-abs(re$TurnoverRate) 
      re <- re[substr(re$ID, 1,3) %in% c('EQ0',"EQ6","EQ3"),]
      re <- arrange(re,ID,TradingDay)
      
      liquidity <- ddply(re,"ID",mutate,STOM=rollsum(TurnoverRate,21,fill=NA,align = 'right'))
      liquidity <- subset(liquidity,!is.na(liquidity$STOM))
      liquidity$STOM <- log(liquidity$STOM)
      liquidity[liquidity$TurnoverRate==0,"STOM"] <- NA
      liquidity <- liquidity[,c("ID","TradingDay","STOM")]
      colnames(liquidity)[3] <- factordf$factorID[i]
      
      result <- liquidity
    }else if(factordf$factorName[i]=='beta'){
      cat('building ',factordf$factorName[i],' factor...\n')
      re <- dbReadTable(con, "QT_DailyQuote", select.cols="ID,TradingDay,DailyReturn")
      re <- re[re$TradingDay>=20031101,]
      re <- arrange(re,ID,TradingDay)
      
      qr <- "SELECT convert(varchar(8),q.[TradingDay],112) 'TradingDay',
              q.ClosePrice/q.PrevClosePrice-1 'indexRtn'
              FROM QT_IndexQuote q,SecuMain s
              where q.InnerCode=s.InnerCode AND s.SecuCode='801003'
              and q.TradingDay>='2003-11-01' and q.TradingDay<='2015-12-31'
              order by q.TradingDay"
      index <- sqlQuery(db.jy(),qr)
      
      tmp <- as.data.frame(table(re$ID))
      tmp <- tmp[tmp$Freq>=250,]
      re <- re[re$ID%in%tmp$Var1,]
      stocks <- unique(re$ID)
      pb <- txtProgressBar(style = 3)
      for(j in 1:length(stocks)){
        tmp <- re[re$ID==stocks[j],]
        tmp <- merge.x(tmp,index,by='TradingDay')
        beta.tmp <- rollapply(tmp[,c('DailyReturn','indexRtn')], width = 250,
                              function(x) coef(lm(DailyReturn ~ indexRtn, data = as.data.frame(x))),
                              by.column = FALSE, align = "right")
        beta.tmp <- as.data.frame(beta.tmp)
        beta.tmp$ID <- stocks[j]
        beta.tmp$TradingDay <- tmp$TradingDay[250:nrow(tmp)]
        beta.tmp <- beta.tmp[,c("ID","TradingDay","indexRtn")]
        colnames(beta.tmp)[3] <- factordf$factorID[i]
        if(j==1){
          beta <- beta.tmp
        }else{
          beta <- rbind(beta,beta.tmp)
        }
        setTxtProgressBar(pb, j/length(stocks))
      }
      close(pb)
      result <- merge.x(result,beta,by=c('ID','TradingDay'))
      result <- result[,c("ID","TradingDay","F000021","F000022")]
    }else if(factordf$factorName[i]=='IVR'){
      cat('building ',factordf$factorName[i],' factor...\n')
      begT <- as.Date('2005-01-31')
      endT <- Sys.Date()-1
      dates <- getRebDates(begT,endT)
    
      getSMB <- function(begT,endT,indexID = "EI000985"){
        RebDate <- getRebDates(begT,endT)
        TS <- getTS(RebDate, indexID)
        TSF <- gf_lcfs(TS,'F000002')
        TSF <- RFactorModel:::factor.na(TSF,method='median')
        TSF <- ddply(TSF, ~ date, mutate, group = as.numeric(cut_number(factorscore,3)))
        tmp.TS1 <- TSF[TSF$group==1,c("date","stockID")]
        tmp.TS1 <- ddply(tmp.TS1, ~ date, mutate, recnum = seq(1,length(date)))
        tmp.TS3 <- TSF[TSF$group==3,c("date","stockID")]
        tmp.TS3 <- ddply(tmp.TS3, ~ date, mutate, recnum = seq(1,length(date)))
        
        tmp <- ddply(tmp.TS1,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS1 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS1 <- ddply(TS1, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS1 <- merge.x(TS1,tmp.TS1)
        TS1 <- TS1[,c("datenew","stockID")]
        colnames(TS1) <- c("date","stockID")
        
        tmp <- ddply(tmp.TS3,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS3 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS3 <- ddply(TS3, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS3 <- merge.x(TS3,tmp.TS3)
        TS3 <- TS3[,c("datenew","stockID")]
        colnames(TS3) <- c("date","stockID")
        
        TSR1 <- getTSR(TS1)
        TSR3 <- getTSR(TS3)
        R1 <- TSR1[!is.na(TSR1$date_end),c("date_end","periodrtn")]
        R1 <- R1[!is.na(R1$periodrtn),]
        R1 <- ddply(R1,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        R3 <- TSR3[!is.na(TSR3$date_end),c("date_end","periodrtn")]
        R3 <- R3[!is.na(R3$periodrtn),]
        R3 <- ddply(R3,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        
        rtn <- merge(R1,R3,by='date_end')
        colnames(rtn) <- c('date','S','B')
        rtn$SMB <- rtn$S-rtn$B
        rtn <- rtn[,c('date','SMB')]
        return(rtn)
      }
      
      getHML <- function(begT,endT,indexID = "EI000985"){
        
        RebDate <- getRebDates(begT,endT)
        TS <- getTS(RebDate, indexID)
        TSF <- gf_lcfs(TS,'F000006')
        TSF <- RFactorModel:::factor.na(TSF,method='median')
        TSF <- ddply(TSF, ~ date, mutate, group = as.numeric(cut_number(factorscore,3)))
        tmp.TS1 <- TSF[TSF$group==1,c("date","stockID")]
        tmp.TS1 <- ddply(tmp.TS1, ~ date, mutate, recnum = seq(1,length(date)))
        tmp.TS3 <- TSF[TSF$group==3,c("date","stockID")]
        tmp.TS3 <- ddply(tmp.TS3, ~ date, mutate, recnum = seq(1,length(date)))
        
        tmp <- ddply(tmp.TS1,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS1 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS1 <- ddply(TS1, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS1 <- merge.x(TS1,tmp.TS1)
        TS1 <- TS1[,c("datenew","stockID")]
        colnames(TS1) <- c("date","stockID")
        
        tmp <- ddply(tmp.TS3,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS3 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS3 <- ddply(TS3, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS3 <- merge.x(TS3,tmp.TS3)
        TS3 <- TS3[,c("datenew","stockID")]
        colnames(TS3) <- c("date","stockID")
        
        TSR1 <- getTSR(TS1)
        TSR3 <- getTSR(TS3)
        R1 <- TSR1[!is.na(TSR1$date_end),c("date_end","periodrtn")]
        R1 <- R1[!is.na(R1$periodrtn),]
        R1 <- ddply(R1,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        R3 <- TSR3[!is.na(TSR3$date_end),c("date_end","periodrtn")]
        R3 <- R3[!is.na(R3$periodrtn),]
        R3 <- ddply(R3,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        
        rtn <- merge(R1,R3,by='date_end')
        colnames(rtn) <- c('date','H','L')
        rtn$HML <- rtn$H-rtn$L
        rtn <- rtn[,c('date','HML')]
        return(rtn)
      }
      
      SMB <- getSMB(begT,endT)
      HML <- getHML(begT,endT)
      
      FF3 <- merge(SMB,HML,by='date')
      qr <- paste("SELECT convert(varchar(8),q.[TradingDay],112) 'date',
                  q.ClosePrice/q.PrevClosePrice-1 'market'
                  FROM QT_IndexQuote q,SecuMain s
                  where q.InnerCode=s.InnerCode AND s.SecuCode='801003'
                  and q.TradingDay>=",QT(min(FF3$date))," and q.TradingDay<=",QT(max(FF3$date)),
                  " order by q.TradingDay")
      index <- sqlQuery(db.jy(),qr)
      index$date <- intdate2r(index$date)
      FF3 <- merge(FF3,index,by='date')
      
      
      qr <- paste("SELECT convert(varchar(8),q.TradingDay,112) 'date',
                  'EQ'+s.SecuCode 'stockID',[ClosePrice]/[PrevClosePrice]-1 'dailyrtn'
                  FROM [JYDB].[dbo].[QT_DailyQuote] q, JYDB.dbo.SecuMain s
                  where s.InnerCode=q.InnerCode and q.TradingDay>=",QT(min(FF3$date)),
                  " and q.TradingDay<=",QT(max(FF3$date))," and s.SecuCategory=1 and s.SecuMarket in (83,90)
                  order by s.SecuCode,q.TradingDay")
      stockrtn <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
      stockrtn$date <- intdate2r(stockrtn$date)
      
      tmp.stock <- unique(stockrtn$stockID)
      IVR <- data.frame()
      pb <- txtProgressBar(style = 3)
      nwin <- 22
      for(i in 1:length(tmp.stock)){
        tmp.rtn <- stockrtn[stockrtn$stockID==tmp.stock[i],]
        tmp.FF3 <- merge(FF3,tmp.rtn[,c('date','dailyrtn')],by='date',all.x=T)
        tmp.FF3 <- tmp.FF3[!is.na(tmp.FF3$dailyrtn),]
        if(nrow(tmp.FF3)<nwin) next
        tmp <- rollapply(tmp.FF3[,c("dailyrtn","market","SMB","HML")], width =nwin,
                         function(x){
                           x <- as.data.frame(x)
                           if(sum(x$dailyrtn==0)>=5){
                             result <- NaN
                           }else{
                             tmp.lm <- lm(dailyrtn~market+SMB+HML, data = x)
                             result <- 1-summary(tmp.lm)$r.squared
                           }
                           return(result)},by.column = FALSE, align = "right")
        IVR.tmp <- data.frame(date=tmp.FF3$date[nwin:nrow(tmp.FF3)],
                              stockID=as.character(tmp.stock[i]),
                              IVRValue=tmp)
        IVR <- rbind(IVR,IVR.tmp)
        setTxtProgressBar(pb, i/length(tmp.stock))
      }
      close(pb)
      IVR <- IVR[!is.nan(IVR$IVRValue),]
      IVR$date <- rdate2int(IVR$date)
      colnames(IVR) <- c('TradingDay','ID','F000023')
      IVR <- IVR[,c('ID','TradingDay','F000023')]
      
      result <- merge.x(result,IVR,by=c('ID','TradingDay'))
      result <- result[,c("ID","TradingDay","F000021","F000022","F000023")]
      result <- arrange(result,TradingDay,ID)
    }
  }
  
  dbWriteTable(db.local(),"QT_FactorScore_amtao",result,overwrite=T,append=F)
  print(Sys.time()-time)
}

#' lcdb.update.QT_FactorScore_amtao
#' 
#' update the QT_FactorScore_amtao table in local database
#' @author Andrew Dow
#' @examples 
#' lcdb.update.QT_FactorScore_amtao()
lcdb.update.QT_FactorScore_amtao <- function(){
  con <- db.local()
  factordf <- dbGetQuery(con,"select factorID,factorName from CT_FactorLists_amtao")
  date <- dbGetQuery(con,"select max(TradingDay) 'date' from QT_FactorScore_amtao")
  dbDisconnect(con)
  date <- intdate2r(date$date)
  
  for(i in 1:nrow(factordf)){
    if(factordf$factorName[i]=='liquidity'){
      cat('updating ',factordf$factorName[i],' factor...\n')
      new.date <- trday.nearby(date,21)
      qr <- paste("select t.ID,t.TradingDay,t.TurnoverVolume,t.NonRestrictedShares 
                  from QT_DailyQuote t
                  where t.TradingDay>=",rdate2int(new.date))
      re <- sqlQuery(db.quant(),qr)
      re$TurnoverRate <- re$TurnoverVolume/(re$NonRestrictedShares*10000)
      re <- re[,c("ID","TradingDay","TurnoverRate")]
      re$TurnoverRate <-abs(re$TurnoverRate) 
      re <- re[substr(re$ID, 1,3) %in% c('EQ0',"EQ6","EQ3"),]
      re <- arrange(re,ID,TradingDay)
      tmp <- as.data.frame(table(re$ID))
      tmp <- tmp[tmp$Freq>=21,]
      re <- re[re$ID%in%tmp$Var1,]
      
      liquidity <- ddply(re,"ID",mutate,STOM=rollsum(TurnoverRate,21,fill=NA,align = 'right'))
      liquidity <- subset(liquidity,!is.na(liquidity$STOM))
      liquidity <- liquidity[liquidity$TradingDay>rdate2int(date),]
      liquidity$STOM <- log(liquidity$STOM)
      liquidity[liquidity$TurnoverRate==0,"STOM"] <- NA
      liquidity <- liquidity[,c("ID","TradingDay","STOM")]
      colnames(liquidity)[3] <- factordf$factorID[i]
      
      result <- liquidity
    }else if(factordf$factorName[i]=='beta'){
      cat('updating ',factordf$factorName[i],' factor...\n')
      new.date <- trday.nearby(date,250)
      qr <- paste("select t.ID,t.TradingDay,t.DailyReturn
                  from QT_DailyQuote t
                  where t.TradingDay>=",rdate2int(new.date))
      re <- sqlQuery(db.quant(),qr)
      re <- arrange(re,ID,TradingDay)
      qr <- paste("SELECT convert(varchar(8),q.[TradingDay],112) 'TradingDay',
                  q.ClosePrice/q.PrevClosePrice-1 'indexRtn'
                  FROM QT_IndexQuote q,SecuMain s
                  where q.InnerCode=s.InnerCode AND s.SecuCode='801003'
                  and q.TradingDay>=",QT(new.date),
                  "order by q.TradingDay")
      index <- sqlQuery(db.jy(),qr)
      re <- merge.x(re,index)
      re <- re[!is.na(re$indexRtn),]
      tmp <- as.data.frame(table(re$ID))
      tmp <- tmp[tmp$Freq>=250,]
      re <- re[re$ID%in%tmp$Var1,]
      re <- arrange(re,ID,TradingDay)
      
      stocks <- unique(re$ID)
      pb <- txtProgressBar(style = 3)
      for(j in 1:length(stocks)){
        tmp <- re[re$ID==stocks[j],]
        beta.tmp <- rollapply(tmp[,c('DailyReturn','indexRtn')], width = 250,
                              function(x) coef(lm(DailyReturn ~ indexRtn, data = as.data.frame(x))),
                              by.column = FALSE, align = "right")
        beta.tmp <- as.data.frame(beta.tmp)
        beta.tmp$ID <- stocks[j]
        beta.tmp$TradingDay <- tmp$TradingDay[250:nrow(tmp)]
        beta.tmp <- beta.tmp[,c("ID","TradingDay","indexRtn")]
        colnames(beta.tmp)[3] <- factordf$factorID[i]
        if(j==1){
          beta <- beta.tmp
        }else{
          beta <- rbind(beta,beta.tmp)
        }
        setTxtProgressBar(pb, j/length(stocks))
      }
      close(pb)
      beta <- beta[beta$TradingDay>rdate2int(date),]
      result <- merge.x(result,beta,by=c('ID','TradingDay'))
      result <- result[,c("ID","TradingDay","F000021","F000022")]
    }else if(factordf$factorName[i]=='IVR'){
      cat('updating ',factordf$factorName[i],' factor...\n')
      begT <- trday.nearby(date,22)
      endT <- Sys.Date()-1
      dates <- getRebDates(begT,endT)
      
      
      getSMB <- function(begT,endT,indexID = "EI000985"){
        RebDate <- getRebDates(begT,endT)
        TS <- getTS(RebDate, indexID)
        TSF <- gf_lcfs(TS,'F000002')
        TSF <- RFactorModel:::factor.na(TSF,method='median')
        TSF <- ddply(TSF, ~ date, mutate, group = as.numeric(cut_number(factorscore,3)))
        tmp.TS1 <- TSF[TSF$group==1,c("date","stockID")]
        tmp.TS1 <- ddply(tmp.TS1, ~ date, mutate, recnum = seq(1,length(date)))
        tmp.TS3 <- TSF[TSF$group==3,c("date","stockID")]
        tmp.TS3 <- ddply(tmp.TS3, ~ date, mutate, recnum = seq(1,length(date)))
        
        tmp <- ddply(tmp.TS1,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS1 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS1 <- ddply(TS1, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS1 <- merge.x(TS1,tmp.TS1)
        TS1 <- TS1[,c("datenew","stockID")]
        colnames(TS1) <- c("date","stockID")
        
        tmp <- ddply(tmp.TS3,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS3 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS3 <- ddply(TS3, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS3 <- merge.x(TS3,tmp.TS3)
        TS3 <- TS3[,c("datenew","stockID")]
        colnames(TS3) <- c("date","stockID")
        
        TSR1 <- getTSR(TS1)
        TSR3 <- getTSR(TS3)
        R1 <- TSR1[!is.na(TSR1$date_end),c("date_end","periodrtn")]
        R1 <- R1[!is.na(R1$periodrtn),]
        R1 <- ddply(R1,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        R3 <- TSR3[!is.na(TSR3$date_end),c("date_end","periodrtn")]
        R3 <- R3[!is.na(R3$periodrtn),]
        R3 <- ddply(R3,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        
        rtn <- merge(R1,R3,by='date_end')
        colnames(rtn) <- c('date','S','B')
        rtn$SMB <- rtn$S-rtn$B
        rtn <- rtn[,c('date','SMB')]
        return(rtn)
      }
      
      getHML <- function(begT,endT,indexID = "EI000985"){
        
        RebDate <- getRebDates(begT,endT)
        TS <- getTS(RebDate, indexID)
        TSF <- gf_lcfs(TS,'F000006')
        TSF <- RFactorModel:::factor.na(TSF,method='median')
        TSF <- ddply(TSF, ~ date, mutate, group = as.numeric(cut_number(factorscore,3)))
        tmp.TS1 <- TSF[TSF$group==1,c("date","stockID")]
        tmp.TS1 <- ddply(tmp.TS1, ~ date, mutate, recnum = seq(1,length(date)))
        tmp.TS3 <- TSF[TSF$group==3,c("date","stockID")]
        tmp.TS3 <- ddply(tmp.TS3, ~ date, mutate, recnum = seq(1,length(date)))
        
        tmp <- ddply(tmp.TS1,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS1 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS1 <- ddply(TS1, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS1 <- merge.x(TS1,tmp.TS1)
        TS1 <- TS1[,c("datenew","stockID")]
        colnames(TS1) <- c("date","stockID")
        
        tmp <- ddply(tmp.TS3,~date,summarise,nstock=length(date))
        tmp.date <- data.frame(date=getRebDates(begT,Sys.Date()-1,rebFreq = 'day'))
        tmp.date$nstock <- tmp$nstock[findInterval(tmp.date$date,tmp$date)]
        tmp.date$datecor <- tmp$date[findInterval(tmp.date$date,tmp$date)]
        TS3 <- data.frame(datenew=rep(tmp.date$date,tmp.date$nstock),
                          date=rep(tmp.date$datecor,tmp.date$nstock))
        TS3 <- ddply(TS3, ~ datenew, mutate, recnum = seq(1,length(datenew)))
        TS3 <- merge.x(TS3,tmp.TS3)
        TS3 <- TS3[,c("datenew","stockID")]
        colnames(TS3) <- c("date","stockID")
        
        TSR1 <- getTSR(TS1)
        TSR3 <- getTSR(TS3)
        R1 <- TSR1[!is.na(TSR1$date_end),c("date_end","periodrtn")]
        R1 <- R1[!is.na(R1$periodrtn),]
        R1 <- ddply(R1,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        R3 <- TSR3[!is.na(TSR3$date_end),c("date_end","periodrtn")]
        R3 <- R3[!is.na(R3$periodrtn),]
        R3 <- ddply(R3,~date_end,summarise,dailtRtn=mean(periodrtn,na.rm = T))
        
        rtn <- merge(R1,R3,by='date_end')
        colnames(rtn) <- c('date','H','L')
        rtn$HML <- rtn$H-rtn$L
        rtn <- rtn[,c('date','HML')]
        return(rtn)
      }
      
      SMB <- getSMB(begT,endT)
      HML <- getHML(begT,endT)
      
      FF3 <- merge(SMB,HML,by='date')
      qr <- paste("SELECT convert(varchar(8),q.[TradingDay],112) 'date',
                  q.ClosePrice/q.PrevClosePrice-1 'market'
                  FROM QT_IndexQuote q,SecuMain s
                  where q.InnerCode=s.InnerCode AND s.SecuCode='801003'
                  and q.TradingDay>=",QT(min(FF3$date))," and q.TradingDay<=",QT(max(FF3$date)),
                  " order by q.TradingDay")
      index <- sqlQuery(db.jy(),qr)
      index$date <- intdate2r(index$date)
      FF3 <- merge(FF3,index,by='date')
      
      
      qr <- paste("SELECT convert(varchar(8),q.TradingDay,112) 'date',
                  'EQ'+s.SecuCode 'stockID',[ClosePrice]/[PrevClosePrice]-1 'dailyrtn'
                  FROM [JYDB].[dbo].[QT_DailyQuote] q, JYDB.dbo.SecuMain s
                  where s.InnerCode=q.InnerCode and q.TradingDay>=",QT(min(FF3$date)),
                  " and q.TradingDay<=",QT(max(FF3$date))," and s.SecuCategory=1 and s.SecuMarket in (83,90)
                  order by s.SecuCode,q.TradingDay")
      stockrtn <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
      stockrtn$date <- intdate2r(stockrtn$date)
      
      tmp.stock <- unique(stockrtn$stockID)
      IVR <- data.frame()
      pb <- txtProgressBar(style = 3)
      nwin <- 22
      for(i in 1:length(tmp.stock)){
        tmp.rtn <- stockrtn[stockrtn$stockID==tmp.stock[i],]
        tmp.FF3 <- merge(FF3,tmp.rtn[,c('date','dailyrtn')],by='date',all.x=T)
        tmp.FF3 <- tmp.FF3[!is.na(tmp.FF3$dailyrtn),]
        if(nrow(tmp.FF3)<nwin) next
        tmp <- rollapply(tmp.FF3[,c("dailyrtn","market","SMB","HML")], width =nwin,
                         function(x){
                           x <- as.data.frame(x)
                           if(sum(x$dailyrtn==0)>=5){
                             result <- NaN
                           }else{
                             tmp.lm <- lm(dailyrtn~market+SMB+HML, data = x)
                             result <- 1-summary(tmp.lm)$r.squared
                           }
                           return(result)},by.column = FALSE, align = "right")
        IVR.tmp <- data.frame(date=tmp.FF3$date[nwin:nrow(tmp.FF3)],
                              stockID=as.character(tmp.stock[i]),
                              IVRValue=tmp)
        IVR <- rbind(IVR,IVR.tmp)
        setTxtProgressBar(pb, i/length(tmp.stock))
      }
      close(pb)
      IVR <- IVR[IVR$date>date,]
      IVR <- IVR[!is.nan(IVR$IVRValue),]
      IVR$date <- rdate2int(IVR$date)
      colnames(IVR) <- c('TradingDay','ID','F000023')
      IVR <- IVR[,c('ID','TradingDay','F000023')]
      
      result <- merge.x(result,IVR,by=c('ID','TradingDay'))
      result <- result[,c("ID","TradingDay","F000021","F000022","F000023")]
      result <- arrange(result,TradingDay,ID)
    }
  }
  
  dbWriteTable(db.local(),"QT_FactorScore_amtao",result,overwrite=F,append=T)
  return('Done!')
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
  
  #get raw data
  qr <- "SELECT 'EQ'+s.SecuCode 'stockID',l.CompanyCode,l.FirstIndustryCode 'Code1',l.FirstIndustryName 'Name1',
  l.SecondIndustryCode 'Code2',l.SecondIndustryName 'Name2',l.ThirdIndustryCode 'Code3',
  l.ThirdIndustryName 'Name3',convert(varchar, l.InfoPublDate, 112) 'InDate',
  convert(varchar, l.CancelDate, 112) 'OutDate',l.InfoSource,l.Standard,l.Industry,
  l.IfPerformed 'Flag',l.XGRQ 'UpdateTime'
  FROM [JYDB].[dbo].[LC_ExgIndustry] l,JYDB.dbo.SecuMain s
  where l.CompanyCode=s.CompanyCode and s.SecuCategory=1 
  and s.SecuMarket in(83,90) and l.Standard in(9,24)"
  re <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
  re <- re[str_sub(re$stockID,1,3) %in% c('EQ6','EQ3','EQ0'),]
  re <- re[ifelse(is.na(re$OutDate),T,re$OutDate!=re$InDate),] # remove indate==outdate wrong data
  
  sw24use <- re[(re$InDate>20140101) & (re$Standard==24),]
  sw9use <- re[(re$InDate<20140101) & (re$Standard==9),]
  sw24tmp <- re[(re$InDate==20140101) & (re$Standard==24),]
  sw9tmp <- sw9use[is.na(sw9use$OutDate) | sw9use$OutDate>20140101,c("stockID","Code1","Name1","Code2","Name2","Code3","Name3")]
  colnames(sw9tmp) <- c("stockID","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")
  hashtable <- merge(sw24tmp,sw9tmp,by='stockID',all.x=T)
  hashtable <- hashtable[,c("Code1","Name1","Code2","Name2","Code3","Name3","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")]
  hashtable <- unique(hashtable)
  hashtable <- ddply(hashtable,~OldName3,mutate,n=length(OldName3))
  hashtable <- hashtable[hashtable$n==1,c("Code1","Name1","Code2","Name2","Code3","Name3","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")]
  
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
  sw9use <- sw9use[!is.na(sw9use$Code1),c("stockID","CompanyCode","Code1","Name1","Code2","Name2",      
                                          "Code3","Name3","InDate","OutDate","InfoSource","Standard", 
                                          "Industry","Flag","UpdateTime")]
  
  tmp <- arrange(tmp,stockID,InDate)
  tmp <-merge(tmp,sw24tmp[,c("stockID","Code1","Name1","Code2","Name2","Code3","Name3")],by='stockID',all.x=T) 
  tmp[is.na(tmp$Code1),c("Name1","Name2","Name3")] <- '综合'
  tmp[is.na(tmp$Code1),"Code1"] <-510000
  tmp[is.na(tmp$Code2),"Code3"] <-510100
  tmp[is.na(tmp$Code3),"Code3"] <-510101
  tmp <- tmp[,c("stockID","CompanyCode","Code1","Name1","Code2","Name2",      
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
  #1 outdate<=indate
  sw33 <- sw33[ifelse(is.na(sw33$OutDate),T,sw33$OutDate>sw33$InDate),]
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
  # 4 duplicate indate
  sw33 <- sw33[ifelse(is.na(sw33$OutDate),T,sw33$OutDate>sw33$InDate),]
  
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
  qr <-"select a.*,b.F000021 'factorscore'
  from yrf_tmp a left join QT_FactorScore_amtao b
  on a.date=b.TradingDay and a.stockID=b.ID"
  re <- dbGetQuery(con,qr)
  dbDisconnect(con)  
  re <- transform(re, date=intdate2r(date))
  re <- arrange(re,date,stockID)
  return(re)
}


#' gf.beta
#'
#' get beta factor in local db
#' @author Andrew Dow
#' @param TS is a TS object.
#' @return a TSF object
#' @examples 
#' gf.beta(TS)
gf.beta <- function(TS){
  check.TS(TS)  
  TS$date <- rdate2int(TS$date)
  con <- db.local()
  dbWriteTable(con,name="yrf_tmp",value=TS[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
  qr <-"select a.*,b.F000022 'factorscore'
  from yrf_tmp a left join QT_FactorScore_amtao b
  on a.date=b.TradingDay and a.stockID=b.ID"
  re <- dbGetQuery(con,qr)
  dbDisconnect(con)  
  re <- transform(re, date=intdate2r(date))
  re <- arrange(re,date,stockID)
  return(re)
}


#' gf.IVR
#'
#' get IVR factor in local db
#' @author Andrew Dow
#' @param TS is a TS object.
#' @return a TSF object
#' @examples 
#' gf.IVR(TS)
gf.IVR <- function(TS){
  check.TS(TS)  
  TS$date <- rdate2int(TS$date)
  con <- db.local()
  dbWriteTable(con,name="yrf_tmp",value=TS[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
  qr <-"select a.*,b.F000023 'factorscore'
  from yrf_tmp a left join QT_FactorScore_amtao b
  on a.date=b.TradingDay and a.stockID=b.ID"
  re <- dbGetQuery(con,qr)
  dbDisconnect(con)  
  re <- transform(re, date=intdate2r(date))
  re <- arrange(re,date,stockID)
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
  TSF <- gf_lcfs(TS, 'F000002')
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
    riskfexp <- as.data.frame(list(...))
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
  cat("This function running time is ",tpassed/60,"min.")
  return(result)
}







