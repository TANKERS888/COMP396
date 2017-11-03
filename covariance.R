#covariance                              

covariance<-function(){
  covmat<-matrix(0,55,2)

  k<-1
  for (i in 1:10) {
    for (j in i:10) {
      m<-dataList[[i]][,"Close"]
      n<-dataList[[j]][,"Close"]
      covmat[k,1]<-paste("[",i,",",j,"]")
      covmat[k,2]<-cov(m,n)
      k<-k+1
  }
  }
  return(covmat)
}