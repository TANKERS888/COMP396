##############################################################
#          seperate and total variance                       #
##############################################################
variance<-function(){

varmat<-matrix(0,10,6) 
colnames(varmat)<-c("variance1-200","variance201-400",
                    "variance401-600","variance601-800",
                    "variance801-1000","intergratedVariance")
  for (i in 1:10) {
#variance of sections
varmat[i,1]<-round(var(dataList[[i]][c(1,200),"Close"]),digits = 3)
varmat[i,2]<-round(var(dataList[[i]][c(201,400),"Close"]),digits = 3)
varmat[i,3]<-round(var(dataList[[i]][c(401,600),"Close"]),digits = 3)
varmat[i,4]<-round(var(dataList[[i]][c(601,800),"Close"]),digits = 3)
varmat[i,5]<-round(var(dataList[[i]][c(801,1000),"Close"]),digits = 3)
#integrated variance
varmat[i,6]<-round(var(dataList[[i]][,"Close"]),digits = 3)
  }
  
plot.zoo(varmat,xlab = c(1,10),ylab = c("1-200","201-400","401-600",
                                        "601-800","801-1000"))
return(varmat)
}