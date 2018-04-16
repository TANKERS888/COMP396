#https://www.zhihu.com/question/19647535 ≈¿’’∆¨
library(rvest)
library(downloader)
library(stringr)
library(dplyr)

#µ•’≈’’∆¨
url<-"https://pic4.zhimg.com/2db250e935ca4f1b8b2b546c60104067_b.jpg"
download(url,"C:/Users/Sheng/Desktop/R/pic/turebbb.jpg", mode = "wb")

#∂‡’≈’’∆¨
url <- 'https://www.zhihu.com/question/19647535'
link<- read_html(url)%>%html_nodes("img")%>% html_attr("src")
pat = "https"
link<-grep(pat, link,value=TRUE)
for(i in 1:length(link)) 

{
  download(link[i],paste("C:/Users/Sheng/Desktop/R/pic",i,".jpg",sep = ""), mode = "wb")
  
} 
