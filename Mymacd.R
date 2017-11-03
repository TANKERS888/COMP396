##############################################
#              Part1 indicators:MACD         #
#                                            #
##############################################
#MACD
maxRows <- 10000
getOrders <- function(store,newRowList,currentPos,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  marketOrders <- allzero
  pos<- allzero
  
  # exit positions from yesterday
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  startIndex <- params$slow + params$signal-1
  #print(startIndex)
  # the valid number is from the 59, iter begins at 0

  if (store$iter >= startIndex) {
    marketOrders <- sapply(1:length(newRowList), function(x)  
      ifelse(x %in% params$series, 
             Mymacd(store$hi,store$lo,store$cl,which(x==params$series),store$iter,params$wprn)
             , 0))
}
  
  
  
  marketOrders <- marketOrders - currentPos 
  #considering change the marketOrders -> -marketOrders
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))}

######################################
# functions for managing the store (high, low, close)

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

############################
dataList[[1]]

Mymacd<-function(hiStore,loStore,clStore,column,iter,n){
  startIndex <-iter - params$slow - params$signal+1
  #print(iter)
  #print(startIndex)
  high<- hiStore[startIndex:iter,column]
  low<- loStore[startIndex:iter,column]
  closeStore<- clStore[startIndex:iter,column]
  dataStore<- cbind(high,low,closeStore)
  macd <- last(MACD(closeStore,
                    nFast=params$fast,nSlow = params$slow,
                    nSig = params$signal,percent = FALSE))
 
  williamsP<- last(WPR(dataStore,n=params$wprn))
  macdLine<- macd["macd"]
  signalLine <- macd["signal"]
  #print(macd)
    if (macdLine < signalLine && williamsP > 0.8)
        return(-1) # short

    if (macdLine > signalLine && williamsP < 0.2)
        return(1)  # long
  return(0) 
  }
  
