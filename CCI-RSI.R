pt<-(dataList[[1]][1:100,"High"]+dataList[[1]][1:100,"Low"]+dataList[[1]][1:100,"Close"])/3
sma<- as.numeric(SMA(pt,n=20)) 
MDevi<-sum(abs(sma - pt))/20