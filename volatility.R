#volitility
volatility<- function(){
  
  volmat<-matrix(0,10,1)
  for (i in 1:10) {
    price<-dataList[[i]]$"Close"
    diff<- diff(log(price))
    diff[1]<-0
    volmat[i,1]<- sqrt(252) * sd(diff) * 100
  }
  
  #plot volatility chart
  plot.zoo(volmat,xlab = "csv",ylab = "vol",lwd = 2, main = "volatility")
  points(volmat,col="red",lwd= 2)
  grid(nx=10,ny=25)
  colnames(volmat)[1] <- c("volatility")
  return(volmat)
}




