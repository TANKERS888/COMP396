##########
#  KDJ   #
##########
maxRows <- 10000 

###################### getOrders ############################

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)

    pos <- allzero
  
  
  if ((store$iter>params$lookback-1)) { 
    
    for (i in 1:length(params$series)) {
      
      startIndex <- store$iter - params$lookback 
      
      cl <- newRowList[[params$series[i]]]$Close
      
      
      kdj <- stoch(cbind(store$hi[startIndex:store$iter,i],
                         store$lo[startIndex:store$iter,i],
                         store$cl[startIndex:store$iter,i]),
                   nFastK = params$nFastK, 
                   nFastD = params$nFastD, 
                   nSlowD = params$nSlowD, 
                   maType = list(list(SMA),list(EMA,wilder=TRUE),list(SMA)), 
                   bounded= TRUE,
                   smooth=1)*100
      
      KDJ <- last(kdj)
    
      J <- 3*as.numeric(KDJ[["fastD"]]) - 2*as.numeric(KDJ[["fastK"]])  #J line
      D <- as.numeric(KDJ[["fastD"]])  #D line
      K <- as.numeric(KDJ[["fastK"]]) #K line
      
      if (K <20 && D<30 && J<0) {
        pos[params$series[i]] <- -(params$size/last(as.numeric(cl)))  
      }
      else if(K >80 && D >70 && J>90)
        pos[params$series[i]] <- (params$size/last(as.numeric(cl)))
    }
  }
  
  marketOrders <-  pos
  
  
  # exit positions from yesterday
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}




######## Managing the store (high, low, close) ##############



#close INITIAL
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,
                    nrow=maxRows,
                    ncol=length(series))
  return(clStore)
}
#high INITIAL
initHiStore  <- function(newRowList,series) {
  hiStore <- matrix(0,
                    nrow=maxRows,
                    ncol=length(series))
  return(hiStore)
}
#low INITIAL
initLoStore  <- function(newRowList,series) {
  loStore <- matrix(0,
                    nrow=maxRows,
                    ncol=length(series))
  return(loStore)
}
#ALL INITIAL
initStore <- function(newRowList,series) {
  return(list(iter=0,
              cl=initClStore(newRowList,series),
              hi=initHiStore(newRowList,series),
              lo=initLoStore(newRowList,series)))
}

#close UPDATE
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

#high UPDATE
updateHiStore <- function(hiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
}

#low UPDATE
updateLoStore <- function(loStore, newRowList, series, iter) {
  for (i in 1:length(series))
    loStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(loStore)
}

#ALL UPDATE
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter) 
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter) 
  return(store)
}
