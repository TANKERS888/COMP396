# Create data for the graph.
x <- c(11, 30, 39, 20)
labels <- c("70��", "80��", "90��", "00��")

# Give the chart file a name.
png(file = "age_title_colours.jpg")

# Plot the chart with title and rainbow color pallet.
pie(x, labels, main = "��������� - ��״ͼ", col = rainbow(length(x)))

# Save the file.
dev.off()






# Create data for the graph.
x <-  c(21, 62, 10,53)
labels <- c("70��", "80��", "90��", "00��")

piepercent<- paste(round(100*x/sum(x), 2), "%")

# Plot the chart.
pie(x, labels = piepercent, main = "��������� - ��״ͼ",col = rainbow(length(x)))
legend("topright", c("70��","80��","90��","00��"), cex = 0.8,
       fill = rainbow(length(x)))
# Save the file.
dev.off()



# Get the library.
library("plotrix")

# Create data for the graph.
x <-  c(21, 62, 10,53)
lbl <- c("70��", "80��", "90��", "00��")

# Give the chart file a name.
png(file = "3d_pie_chart.jpg")

# Plot the chart.
pie3D(x,labels = lbl,explode = 0.1, main = "��������� - ��״ͼ")

# Save the file.
dev.off()





#QQ 
rd=rnorm(10) 
plot(density(rd),main = "��̬������������ܶ�",lwd=2) 
points(rd,rep(0.01,10),pch=20,col=rainbow(100))

t=rank(rd)/10 #��۲��ۻ�����
q=qnorm(t) 
#���ۻ��������λ��ֵ
plot(rd,q,main = "Q-Qͼ",pch=20,col=rainbow(100)) #��Q-Qͼ
abline(0,1,lwd=2)




#PM2.5
library(XML)
library(leafletCN)
# ��ȡ��ҳ�ı���
# Sorry for ���������վ
table = readHTMLTable("http://www.pm25.in/rank",encoding = "UTF-8", stringsAsFactors = F)[[1]]

# �������ݲ�����
dat = table[ , 2:3]
names(dat) = c("city","AQI")
dat$AQI = as.numeric(dat$AQI)

# ����geojsonMap���л���
geojsonMap(dat, "city",
           popup =  paste0(dat$city, ":", dat$AQI),
           palette = "Reds", legendTitle = "AQI")

filePath = system.file("geojson/china.json",package = "leafletCN")
map = read.geoShape(filePath)