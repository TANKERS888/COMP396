source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/copycat.R') 

#################################################################
# DATA
#################################################################
dataList <- getData(directory="PART1")

#################################################################
# STRATEGY
#################################################################
strategyFile <-'strategies/fixed.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

################################################################
# BACKTEST PARAMETERS
#numOfDays <- 200 # don't use all available days to start with!
#dataList  <- lapply(dataList, function(x) x[1:numOfDays])
#dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

sMult <- 0.20 # slippage multiplier

openDiffs <- lapply(dataList,function(x) diff(x$Open))

toPlot <- do.call(cbind,openDiffs)
colnames(toPlot) <- paste("Series",sprintf("%02d",1:10)) 

plot.zoo(toPlot,
         main="Open on open simple differences",
         cex.axis=1.2,
         cex.main=2,
         yax.flip=TRUE)


absOpenDiffs    <- lapply(openDiffs,abs)
avgAbsDiffs <- sapply(absOpenDiffs,mean,na.rm=TRUE)

largestAvgAbsDiffs <- max(avgAbsDiffs)
positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
params<- list(sizes=positionSizes) # inversely proportional to average abs diff
print(params)