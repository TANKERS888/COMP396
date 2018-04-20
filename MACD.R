library(xts) # this also loads zoo which has read.zoo
library(quantmod)
library(ggplot2)
library(grid)
library(gridExtra)

high<- dataList[[9]][800:1000,"High"]
low<- dataList[[9]][800:1000,"Low"]
close<- dataList[[9]][800:1000,"Close"]

dataStore<- cbind(high,low,close)

williamsP<- WPR(dataStore)
plot(williamsP)

abline(h=0.2,col="red")
abline(h=0.8,col="green")

colnames(KDJ) <- c("K","D","J")
lines(KDJ$K,col="blue",lwd=1.5) 
lines(KDJ$D,col="green",lwd=1.5) 
lines(KDJ$J,col="red",lwd=1.5) 

legend("bottomleft", 
       legend = c( "Overbought","Oversold"), 
       col = c( 
               "green",
               "red"), 
       
       lwd = c(2,2), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

plot((closing))
####################################
par(mfrow=c(2,1))
closing<-dataList[[9]][800:1000,"Close"]
macd =MACD(dataList[[9]][800:1000,"Close"], nFast=12, nSlow=26,nSig=9,matype=SMA)


x1<-macd$macd
x2<-macd$signal

set.seed(1)
x1=data.matrix(as.data.frame(macd$macd))
x2=data.matrix(as.data.frame(macd$signal))
# Find points where x2 is above x1.
above<-x1<x2
# Points always intersect when above=TRUE, then FALSE or reverse
intersect.points<-which(diff(above)!=0)
# Find the slopes for each line segment.
x1.slopes<-x1[intersect.points+1]-x1[intersect.points]
x2.slopes<-x2[intersect.points+1]-x2[intersect.points]
# Find the intersection for each segment.
x.points<-intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes-x2.slopes))
y.points<-x1[intersect.points] + (x1.slopes*(x.points-intersect.points))
# Plot.
plot(x1,type='l')
lines(x2,type='l',col='red')
m<-round(x.points, digits = 0)
n<-y.points
M<-matrix(m)
N<-matrix(n)
mn<-cbind(M,N)
points(m,y.points,col='blue')
plot(closing)

#################################




  
