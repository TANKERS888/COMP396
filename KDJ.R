library(xts) # this also loads zoo which has read.zoo
library(quantmod)
library(ggplot2)
library(grid)
library(gridExtra)
par(mfrow=c(1,1))



kdj <- stoch(cbind(dataList[[9]][800:1000,"High"],
                   dataList[[9]][800:1000,"Low"],
                   dataList[[9]][800:1000,"Close"]),
             nFastK = params$nFastK, 
             nFastD = params$nFastD, 
             nSlowD = params$nSlowD, 
             maType = list(list(SMA),list(EMA,wilder=TRUE),list(SMA)), 
             bounded= TRUE,
             smooth=1)*100


KDJ <-cbind(kdj$fastK,kdj$fastD,(3*kdj$fastD-2*kdj$fastK))
plot(KDJ)
colnames(KDJ) <- c("K","D","J")
lines(KDJ$K,col="blue",lwd=1.5) 
lines(KDJ$D,col="green",lwd=1.5) 
lines(KDJ$J,col="red",lwd=1.5) 
abline(h=0,col="red")
abline(h=100,col="red")
abline(h=70,col="green")
abline(h=80,col="blue")
abline(h=20,col="blue")
abline(h=30,col="green")
legend("bottomleft", 
       legend = c("K", "D","J"), 
       col = c("blue", 
               "green",
               "red"), 
      
       lwd = c(2,2,2), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
