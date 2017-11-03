#kdj

maxRows <- 3300 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  
  if ((store$iter>params$lookback-1)) { 
    
    for (i in 1:length(params$series)) {
      
      startIndex <- store$iter - params$lookback 
      cl <- newRowList[[params$series[i]]]$Close
      
      kdj <- stoch(cbind(store$hg[startIndex:store$iter,i],store$lw[startIndex:store$iter,i],
                         store$cl[startIndex:store$iter,i]),
                   nFastK = params$nFastK, nFastD = params$nFastD, nSlowD = params$nSlowD, 
                   maType = list(list(SMA),list(EMA,wilder=TRUE),list(SMA)), bounded= TRUE,smooth=1)*100
      KDJ <- last(kdj)
      
      
      fastJ <- 3*as.numeric(KDJ[["fastD"]]) - 2*as.numeric(KDJ[["fastK"]])     
      if (fastJ < 0) {
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
      }
      else if (fastJ > 100){
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      }
      else if (as.numeric(KDJ[["fastK"]]) < 20){ # <50
       # if (as.numeric(KDJ[["fastK"]]) < 20 ){
          pos[params$series[i]] <- -params$posSizes[params$series[i]]
        }
        #else if(as.numeric(KDJ[["fastD"]])==as.numeric(KDJ[["fastK"]]) ){
        # pos[params$series[i]] <- -params$posSizes[params$series[i]]
        #}
      #} 
      else if (as.numeric(KDJ[["fastK"]]) > 80){  # >=50
       # if (as.numeric(KDJ[["fastK"]]) >80 ){
          pos[params$series[i]] <- params$posSizes[params$series[i]]
        }
        else if(as.numeric(KDJ[["fastD"]])==as.numeric(KDJ[["fastK"]]) ){
          pos[params$series[i]] <- params$posSizes[params$series[i]]
       # }
      }
    }
  }
  
  marketOrders <- marketOrders + pos
  
  
  # exit positions from yesterday
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}


####################################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
####################################################################################
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}

updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              hg=initHgStore(newRowList,series),
              lw=initLwStore(newRowList,series)))
}

updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$hg <- updateClStore(store$hg,newRowList,series,store$iter) 
  store$lw <- updateClStore(store$lw,newRowList,series,store$iter) 
  return(store)
}

initHgStore  <- function(newRowList,series) {
  hgStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(hgStore)
}

initLwStore  <- function(newRowList,series) {
  lwStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(lwStore)
}

updateHgStore <- function(hgStore, newRowList, series, iter) {
  for (i in 1:length(series))
    hgStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(hgStore)
}

updateLwStore <- function(lwStore, newRowList, series, iter) {
  for (i in 1:length(series))
    lwStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(lwStore)
}
