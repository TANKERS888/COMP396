##############################################
#              Part1 indicators:CCI          #
#              a simplified one              #
#   not finished yet, but finished in part2  #
##############################################
maxRows <- 10000 
getOrders <- function(store, newRowList, currentPos, params) {
  
  if (is.null(store)) {
   
    store <- initStore(newRowList, params$series)
  }
  else 
    store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- rep(0,length(newRowList))
  limitOrders1 <- rep(0,length(newRowList))
  limitPrices1 <- rep(0,length(newRowList))
  limitOrders2 <- rep(0,length(newRowList))
  limitPrices2 <- rep(0,length(newRowList))
  
  
  if (store$iter > params$lookback ) {
    marketOrders   <- sapply(1:length(newRowList),
                             function(x) ifelse(x %in% params$series, 
                                                CCI(store$hi,store$lo,store$cl,which(x==params$series),store$iter), 0))
  }
  
  # exit positions from yesterday
  marketOrders <- marketOrders - currentPos 
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}

#########################################################
# functions for managing the store

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initHiStore  <- function(newRowList,series) {
  hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
initLoStore  <- function(newRowList,series) {
  loStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(loStore)
}


updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateHiStore <- function(hiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
}
updateLoStore <- function(loStore, newRowList, series, iter) {
  for (i in 1:length(series))
    loStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(loStore)
}

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
                    hi=initHiStore(newRowList,series),
                    lo=initLoStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter) 
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter) 
  return(store)
}

###############################################################################

# main strategy logic

CCI <-	function(hiStore,loStore,clStore,column,iter) {
  # decide if we should go long/short/flat (returning 1/-1/0)
  startIndex <- iter - params$lookback - 1
  cciStore<- cbind(hiStore,loStore,clStore)
 # print(cciStore)
  cci <- last(CCI(cciStore[startIndex:iter,column],n=params$lookback)) 
 # print(cci)
  if (cci < -100)
    return(-1) # short
  if (cci > 100)
    return(1)  # long
  return(0)
}
# Note: unfinished, will complete in next stage.
