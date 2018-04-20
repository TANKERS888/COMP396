library(xts) # this also loads zoo which has read.zoo


toDate <- function(x) as.Date(x, origin = "21-02-2052")
z10 <- read.zoo("/Users/chijinsheng/Desktop/r/DATA/PART1/10.csv", header = TRUE, sep = ",", FUN = toDate)
series10 <- as.xts(z10)
library(quantmod)
chartSeries(series10,theme='white')

z7 <- read.zoo("/Users/chijinsheng/Desktop/r/DATA/PART1/07.csv", header = TRUE, sep = ",", FUN = toDate)
serives7 <- as.xts(z7)
library(quantmod)
chartSeries(series7,theme='white')

#-------------------return

simple_diff10 <- diff(series10$Close)
simple_ret10 <- ROC(series10$Close,type='discrete')
log_ret10 <- ROC(series10$Close,type='continuous')
prices10 <- cbind(series10$Close,simple_diff10,simple_ret10,log_ret10)
prices10 <- round(prices10,3) # only show 3 decimal places
colnames(prices10)[1:4] <- c("Close", "Simple Diff","Simple Ret","Log Ret")
colnames(prices10)[1:1]

#-------------------
library(TTR)
par(mfrow=c(2,2))
returns_log10 <- ROC(series10$Close,type='continuous')
returns_simple10 <- ROC(series10$Close,type='discrete')
plot(returns_log10)
hist(returns_log10, breaks = 50, col="brown")
plot(returns_simple10)
hist(returns_simple10, breaks = 50, col="brown")
acf(abs(returns_simple10[-1]), main = "Autocorrelation of absolute returns")

#---------------------
Volatility<-function(){

price10 <- series10$Close
diff10<- diff(log(price10))
diff10[1]<-0
vol<- sqrt(252) * sd(diff10) * 100 # historical vol
vol_percent = sd(price10) / mean(price10)* 100 #present vol

print(vol)
print(vol_percent)

}

getData  <- function(directory) {
  directory <- file.path("DATA",directory) 
  nseries <- length(list.files(directory))
  fnames <- sapply(1:nseries,  function(x) 
    getFilename(directory,index=x))
  cat("Read",nseries, "series from", directory,"\n")
  return(lapply(fnames, readFile))
}

volatility<- function(){
  volmat<-matrix(0,10,1)
  dataList <- getData(directory="PART1")
  for (i in 1:10) {
    volmat[i,1]<-sd(dataList[[i]]$"Close")
    rownames(volmat) <- c("01.csv","02.csv","03.csv","04.csv","05.csv",
                          "06.csv","07.csv","08.csv","09.csv","10.csv")
    
  }
  # ten series volitility together
  plot.zoo(volmat,xlab = "csv",ylab = "standard deviation",lwd = 3)
  points(volmat,col="red",lwd= 2)
  grid(nx=10,ny=25)
  #output ten volatility  
  return(volmat)
}


#--------------------stock simulate
N <- 1000
start_price <- 1000
p <- c(start_price, rep(NA, N-1))
for(i in 2:N)
  p[i] <- p[i-1] * exp(rnorm(1, mu, sigma))

plot(p, type = "l", col = "brown", main = "Simulated Stock Price")

cat('nigesb')


