source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/macd-kdj.R') 

numOfDays <- 100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#MACD seqs
slowSeq <- seq(from=26,to=26,by=20)
fastSeq  <- seq(from=20,to=20,by=15) 
sigSeq  <- seq(from=10,to=10,by=15)
#KDJ seqs
slowDseq<- seq(from=26,to=26,by=20)
fastKSeq<- seq(from=12,to=12,by=15)
fastDseq<- seq(from=9,to=9,by=15)



paramsList  <- list(slowSeq,fastSeq,sigSeq,slowDseq,fastKSeq,fastDseq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=7)
colnames(resultsMatrix) <- c("slow","fast","signal","slowD","fastk","fastD","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (sl in slowSeq) {
  for (fa in fastSeq) {
    for (sig in sigSeq) {
      for (sd in slowDseq) {
        for (fk in fastKSeq) {
          for (fd in fastDseq) {
           
       params <- list(series=1,
                      slow=sl,fast=fa,signal=sig,
                      nSlowD=sd,nFastK=fk,nFastD=fd,
                      posSizes=c(92,23,25,1,11,20,6759,1,43,7),
                      lookback=50,size=300000) 
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results)
        resultsMatrix[count,] <- c(sl,fa,sig,sd,fk,fd,pfolioPnL$fitAgg)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1
    }}}}}}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
