##########
#  MACD  #
##########

maxRows <- 10000

###################### getOrders ############################
getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  marketOrders <- allzero
  
  
  if (is.null(store)){
    store <- initStore(newRowList,
                       params$series)
  }
  else{
    store <- updateStore(store, 
                         newRowList,
                         params$series)
  }
    
  
  startIndex <- params$slow + params$signal-1 # the valid number is from the 26+9-1=34, iter begins at 0
  
  
  
  if (store$iter >= startIndex) {
    marketOrders <- sapply(1:length(newRowList), function(x)  
      ifelse(x %in% params$series,  getMACD(store$hi,
                                            store$lo,
                                            store$cl,
                                            which(x==params$series),
                                            store$iter,
                                            info),
             0))
  }
  
 
  
  marketOrders <- marketOrders - currentPos 
  #considering change the marketOrders -> -marketOrders
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))}


#################### MACD && WPR #########################


getMACD<-function(hiStore,loStore,clStore,column,iter,info){
  startIndex <- 1
  
  high<- hiStore[startIndex:iter,column]
  low<- loStore[startIndex:iter,column]
  close<- clStore[startIndex:iter ,column]
  dataStore<- cbind(high,low,close)
  
  williamsP<- last(WPR(dataStore))
  
  lastCloseStore<- clStore[startIndex:iter+1 ,column]
  lastmacd <- last(MACD(lastCloseStore, 
                        nFast=params$fast,
                        nSlow = params$slow, 
                        nSig = params$signal,
                        maType = SMA))
  
  lastMacdLine<- lastmacd["macd"]
  lastSignalLine <- lastmacd["signal"]
  
  
  
  beforeCloseStore<- clStore[startIndex:iter,column]
  beforeMacd <- last(MACD(beforeCloseStore,
                          nFast=params$fast,
                          nSlow = params$slow, 
                          nSig = params$signal,
                          maType = SMA))
  
  beforeMacdLine<- beforeMacd["macd"]
  beforeSignalLine <- beforeMacd["signal"]

  
  
  if(info$balance+info$netWorth >1000000*(1+params$profitTarget)){ #profit target
      return(0)
  }
  else{
  if (beforeMacd > beforeSignalLine && lastMacdLine < lastSignalLine && williamsP <0.2)
    
    return( floor(-params$size/last(close))) # short 
  
  if (beforeMacd < beforeSignalLine && lastMacdLine > lastSignalLine &&  williamsP >0.8)
    
    return(floor(params$size/last(close)))  # long 
  
  return(0) 
  }
}




######## Managing the store (high, low, close) ##########

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




