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








#' calcfres
#'
#' calculate factor return f data and the residual data
#' @rdname calcfres
#' @author Andrew Dow
#' @param TSFR is a TSFR object.
#' @param regresstype choose the regress type,the default type is glm.
#' @param sectorAttr
#' @param alphafname a charactor string vector. IF missing, then the alphaf and riskf with be mixed together.
#' @return calcfresbyTS return a list, contains alphaf,riskf,residual.
#' @examples 
#' calcfresbyTSFR(TSFR)
calcfresbyTSFR <- function(TSFR,regresstype=c('glm','lm'),sectorAttr=defaultSectorAttr,alphafname){
  ptm <- proc.time()
  regresstype <- match.arg(regresstype)
  
  TS <- TSFR[,c('date','stockID')]
  
  cat("getting sector id......","\n")
  TSS <- getSectorID(TS,sectorAttr = sectorAttr)
  TSS <- sectorNA_fill(TSS,sectorAttr=sectorAttr)
  TSS <- reshape2::dcast(TSS,date+stockID~sector,length,fill=0,value.var = 'sector')
  
  #merge data
  TSFR <- merge(TSFR,TSS,by =c("date","stockID"))
  
  cat("calculating f and residual......","\n")
  if(regresstype=='glm'){
    #get liquid market value
    TSFv <- getTSF(TS,'gf.float_cap',factorStd = 'none',factorNA = "median")
    TSFv$factorscore <- sqrt(TSFv$factorscore)
    TSFv <- dplyr::rename(TSFv,sqrtFV=factorscore)
    TSFRv <- merge(TSFR,TSFv,by =c("date","stockID"))
    dates <- unique(TSFRv$date)
    for(i in 1:(length(dates)-1)){
      tmp.tsfr <- TSFRv[TSFRv$date==dates[i],]
      tmp.x <- as.matrix(dplyr::select(tmp.tsfr,-stockID,-date,-date_end,-periodrtn,-sqrtFV))
      tmp.x <- subset(tmp.x,select = (colnames(tmp.x)[colSums(tmp.x)!=0]))
      tmp.r <- as.matrix(tmp.tsfr[,"periodrtn"])
      tmp.r[is.na(tmp.r)] <- mean(tmp.r,na.rm = T)
      tmp.w <- as.matrix(tmp.tsfr[,"sqrtFV"])
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
  
  if(missing(alphafname)){
    re <- list(f,residual)
  } else {
    alphaf <- f[f$fname %in% alphafname,]
    riskf <- f[!(f$fname %in% alphafname),]
    re <- list(alphaf,riskf,residual)
  }
  
  tpassed <- proc.time()-ptm
  tpassed <- tpassed[3]
  cat("This function running time is ",tpassed/60,"min.")
  return(re)
}


#' @rdname calcfres
#' @param TS
#' @param alphafactorLists is a set of alpha factors for regressing.
#' @param riskfactorLists is a set of risk factors for regressing.
#' @param dure
#' @return calcfresbyTSFR return a list contains TSFR,alphaf,riskf,residual.
#' @export
#' @examples 
#' calcfresbyTS(TS,alphafactorLists,riskfactorLists)
calcfresbyTS <- function(TS,alphafactorLists,riskfactorLists,regresstype=c('glm','lm'),
                         sectorAttr=defaultSectorAttr(),
                         dure=months(1)){
  factorLists <- c(alphafactorLists,riskfactorLists)
  TSF <- getMultiFactor(TS,FactorLists = factorLists)
  TSFR <- getTSR(TSF,dure=dure)
  fres <- calcfresbyTSFR(TSFR = TSFR,regresstype = regresstype,sectorAttr = sectorAttr,alphafname=sapply(alphafactorLists,'[[','factorName'))
  re <- c(list(TSFR),fres)
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
  f <- reshape2::dcast(riskf,date~fname,fill=0,value.var = 'fvalue')
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
      sec <- getSectorID(benchmarkdata[,c('date','stockID')],sectorAttr = sectorAttr)
      sec <- sectorNA_fill(sec,sectorAttr=sectorAttr)
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
      tmp.sec <- getSectorID(tmp[,c('date','stockID')],sectorAttr = sectorAttr)
      tmp.sec <- sectorNA_fill(tmp.sec,sectorAttr=sectorAttr)
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
        sec <- getSectorID(benchmarkdata[,c('date','stockID')],sectorAttr = sectorAttr)
        sec <- sectorNA_fill(sec,sectorAttr=sectorAttr)
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
        tmp.sec <- getSectorID(tmp[,c('date','stockID')],sectorAttr = sectorAttr)
        tmp.sec <- sectorNA_fill(tmp.sec,sectorAttr=sectorAttr)
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







