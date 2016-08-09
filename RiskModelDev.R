#load packages
suppressMessages(library(quantbox))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))
suppressMessages(library(plyr))
suppressMessages(library(stringr))
suppressMessages(library(quadprog))
suppressMessages(library(nloptr))
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


#' check risky factor
#'
#' calculate factor return f data and the residual data
#' @author Andrew Dow
#' @param TS is a TS object.
#' @param riskfactorLists is a set of risk factors for regressing.
#' @return a list,contains TSFR,alphaf,riskf,residual
#' @examples
#' RebDates <- getRebDates(as.Date('2009-12-31'),as.Date('2016-06-30'),rebFreq = 'week')
#' TS <- getTS(RebDates,'EI000985')
#' riskfactorLists <- buildFactorLists(
#' buildFactorList(factorFun = "gf.ln_mkt_cap",factorDir = -1,factorNA = "median",factorStd = "norm")
#' )
#' factorIDs <- c("F000006","F000015","F000016")
#' tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' riskfactorLists <- c(riskfactorLists,tmp)
#' risk.factor.test
#' @export
risk.factor.test <- function(TS,alphafactorLists,riskfactorLists){
  alphaname <- sapply(alphafactorLists,'[[','factorName')
  riskname <- sapply(riskfactorLists,'[[','factorName')
  dates <- unique(TS$date)
  pb <- txtProgressBar(style=3)
  for (i in 1:length(dates)) {
    setTxtProgressBar(pb, i/length(dates))
    tmp.TS <- TS[TS$date==dates[i],]
    TSF <- getMultiFactor(tmp.TS,c(alphafactorLists,riskfactorLists))
    TSF <- TSF[,c('date','stockID',alphaname,riskname)]
    
    TSS <- getSectorID(tmp.TS,sectorAttr = list(33,1))
    TSS[is.na(TSS$sector),'sector'] <- 'ES33510000'
    TSS <- dcast(TSS,date+stockID~sector,length,fill=0,value.var = 'sector')
    indname <- colnames(TSS)[-1:-2]
    
    TSR <- getTSR(tmp.TS,months(3))
    
    #merge data
    TSF <- merge(TSF,TSS,by=c('date','stockID'),all.x=T)
    TSFR <- merge(TSF,TSR,by=c('date','stockID'),all.x=T)
    TSFR <- na.omit(TSFR)
    if(nrow(TSFR)==0) next
    TSFR <- arrange(TSFR,stockID)
    
    wgt <- getTSF(TSFR[,c('date','stockID')],'gf_lcfs',factorPar = list(factorID='F000001'),
                   factorStd = 'none',factorNA = "median")
    wgt$factorscore <- sqrt(wgt$factorscore)
    wgt <- arrange(wgt,stockID)
    
    #industry only
    fname <- indname
    indrsquare <- getrsquare(TSFR,fname,wgt)
    colnames(indrsquare)[-1] <- c('indlm','indglm')
    
    #industry size
    fname <- c("ln_mkt_cap_",indname)
    indsizersquare <- getrsquare(TSFR,fname,wgt)
    colnames(indsizersquare)[-1] <- c('indsizelm','indsizeglm')
    
    #industry risk
    fname <- c(riskname,indname)
    indriskrsquare <- getrsquare(TSFR,fname,wgt)
    colnames(indriskrsquare)[-1] <- c('indrisklm','indriskglm')
    
    #industry risk alpha
    fname <- c(alphaname,riskname,indname)
    indallrsquare <- getrsquare(TSFR,fname,wgt)
    colnames(indallrsquare)[-1] <- c('indalllm','indallglm')
    if(i==1){
      rsquare <- cbind(indrsquare,indsizersquare[,-1],indriskrsquare[,-1],indallrsquare[,-1])
    }else{
      tmp <- cbind(indrsquare,indsizersquare[,-1],indriskrsquare[,-1],indallrsquare[,-1])
      rsquare <- rbind(rsquare,tmp)
    }
    
    
  }
  close(pb)

  
  
}


rs <- xts(rsquare[,-1],order.by = rsquare[,1])
rs <- rollmean(rs,12)
ggplot.ts.line(rs)

getrsquare <- function(TSFR,fname,wgt){
  tmp.x <- as.matrix(TSFR[,fname])
  tmp.x <- subset(tmp.x,select = (colnames(tmp.x)[colSums(tmp.x)!=0]))
  tmp.r <- as.matrix(TSFR[,"periodrtn"])
  tmp.w <- as.matrix(wgt[,"factorscore"])
  tmp.w <- diag(c(tmp.w),length(tmp.w))
  lm.f <- solve(t(tmp.x) %*% tmp.x) %*% t(tmp.x) %*% tmp.r
  glm.f <- solve(crossprod(tmp.x,tmp.w) %*% tmp.x) %*% crossprod(tmp.x,tmp.w) %*% tmp.r
  
  lm.res <- tmp.r-tmp.x %*% lm.f
  glm.res <- tmp.r-tmp.x %*% glm.f
  lm.rsquare <- 1-sum(lm.res^2)/sum((tmp.r-mean(tmp.r))^2)
  glm.rsquare <- 1-sum(glm.res^2)/sum((tmp.r-mean(tmp.r))^2)

  rsquare <- data.frame(date=TSFR$date_end[1],lm=lm.rsquare,glm=glm.rsquare)
  return(rsquare)
}





#' get alpha factor's historical VIF 
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
#' @export
factor.VIF <- function(TS,riskfactorLists){
  
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







