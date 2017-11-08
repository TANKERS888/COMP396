##########
#  MACD  #
##########

maxRows <- 10000

######################get orders############################
getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  marketOrders <- allzero
  
  
  
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  
  startIndex <- params$slow + params$signal-1 # the valid number is from the 26+9-1=34, iter begins at 0
  
  
  
  if (store$iter >= startIndex) {
    marketOrders <- sapply(1:length(newRowList), function(x)  
      ifelse(x %in% params$series,  getMACD(store$hi,store$lo,store$cl,which(x==params$series),store$iter), 0))
  }
  
  
  
  marketOrders <- marketOrders - currentPos 
  #considering change the marketOrders -> -marketOrders
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))}


################## MACD && WPR #########################


getMACD<-function(hiStore,loStore,clStore,column,iter){
  startIndex <- 1
  
  high<- hiStore[startIndex:iter,column]
  low<- loStore[startIndex:iter,column]
  close<- clStore[startIndex:iter ,column]
  dataStore<- cbind(high,low,close)
  
  williamsP<- last(WPR(dataStore))
  
  lastCloseStore<- clStore[startIndex:iter+1 ,column]
  
  lastmacd <- last(MACD(lastCloseStore, nFast=params$fast,nSlow = params$slow, nSig = params$signal,maType = SMA))
  
  lastMacdLine<- lastmacd["macd"]
  lastSignalLine <- lastmacd["signal"]
  
  beforeCloseStore<- clStore[startIndex:iter,column]
  beforeMacd <- last(MACD(beforeCloseStore, nFast=params$fast,nSlow = params$slow, nSig = params$signal,maType = SMA))
  
  beforeMacdLine<- beforeMacd["macd"]
  beforeSignalLine <- beforeMacd["signal"]
  
  if (beforeMacd > beforeSignalLine && lastMacdLine < lastSignalLine && williamsP <0.2)
    
    return( floor(-1000000/last(close))) # short 
  
  if (beforeMacd < beforeSignalLine && lastMacdLine > lastSignalLine &&  williamsP >0.8)
    return( floor(-1000000/last(close)))  # long
  
  return(0) 
}
