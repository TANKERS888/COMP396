#correlation and p-value                       

list1 <- list(dataList[[1]],dataList[[2]],
             dataList[[3]],dataList[[4]],
             dataList[[5]],dataList[[6]],
             dataList[[7]],dataList[[8]],
             dataList[[9]],dataList[[10]]) #sample list

correlation<-function(list){
  
  cormat<-matrix(0,55,3)
  colnames(cormat)<-c("objects","correlation","p-value")
  k<-1
  for (i in 1:10) {
    for (j in i:10) {
      x<-list[[i]][,"Close"]
      y<-list[[j]][,"Close"]
      
      cor<-cor.test(x,y)
      pvalue<-cor[[3]]
      cor<-cor[[4]]
      
      cormat[k,1]<-paste("[",i,",",j,"]")
      cormat[k,2]<-cor
      cormat[k,3]<-pvalue
      k<-k+1
    }
  }
  return(cormat)
}



