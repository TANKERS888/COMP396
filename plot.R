# Create data for the graph.
x <- c(11, 30, 39, 20)
labels <- c("70后", "80后", "90后", "00后")

# Give the chart file a name.
png(file = "age_title_colours.jpg")

# Plot the chart with title and rainbow color pallet.
pie(x, labels, main = "出生年龄段 - 饼状图", col = rainbow(length(x)))

# Save the file.
dev.off()






# Create data for the graph.
x <-  c(21, 62, 10,53)
labels <- c("70后", "80后", "90后", "00后")

piepercent<- paste(round(100*x/sum(x), 2), "%")

# Plot the chart.
pie(x, labels = piepercent, main = "出生年龄段 - 饼状图",col = rainbow(length(x)))
legend("topright", c("70后","80后","90后","00后"), cex = 0.8,
       fill = rainbow(length(x)))
# Save the file.
dev.off()



# Get the library.
library("plotrix")

# Create data for the graph.
x <-  c(21, 62, 10,53)
lbl <- c("70后", "80后", "90后", "00后")

# Give the chart file a name.
png(file = "3d_pie_chart.jpg")

# Plot the chart.
pie3D(x,labels = lbl,explode = 0.1, main = "出生年龄段 - 饼状图")

# Save the file.
dev.off()





#QQ 
rd=rnorm(10) 
plot(density(rd),main = "正态随机变量概率密度",lwd=2) 
points(rd,rep(0.01,10),pch=20,col=rainbow(100))

t=rank(rd)/10 #求观察累积概率
q=qnorm(t) 
#用累积概率求分位数值
plot(rd,q,main = "Q-Q图",pch=20,col=rainbow(100)) #画Q-Q图
abline(0,1,lwd=2)




#PM2.5
library(XML)
library(leafletCN)
# 读取网页的表格
# Sorry for 爬了你家网站
table = readHTMLTable("http://www.pm25.in/rank",encoding = "UTF-8", stringsAsFactors = F)[[1]]

# 整理数据并命名
dat = table[ , 2:3]
names(dat) = c("city","AQI")
dat$AQI = as.numeric(dat$AQI)

# 调用geojsonMap进行绘制
geojsonMap(dat, "city",
           popup =  paste0(dat$city, ":", dat$AQI),
           palette = "Reds", legendTitle = "AQI")

filePath = system.file("geojson/china.json",package = "leafletCN")
map = read.geoShape(filePath)
