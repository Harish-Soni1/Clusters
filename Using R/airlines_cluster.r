rm(list=ls())
cat("\14")

library(readxl)
air_data<-read_xlsx("E:/itsstudytym/assignments/Cluster/EastWestAirlines C.xlsx",sheet="data")
str(air_data)


#######   heirarchical cluster ######


##normalization
air_data<-scale(air_data[,2:12])


##Computing the distance matrix
d<-dist(air_data,method = "euclidean")
fit<-hclust(d,method = "average")


##display dendogram
plot(fit)
groups<-cutree(fit,k=4)


##draw dendogram with red borders around the 4 clusters 
rect.hclust(fit,k=4,border="red")


#Attach the cluster numbers to Uni
clusters=data.frame(air_data,'Cluster' =groups)
clusters


###### k means clster


## Elbow chart
wss<-c()
for(i in 2:15) wss[i]<-sum(kmeans(air_data,centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "passenger")


### Cluster algorithm building
km <- kmeans(air_data,8) 
km$centers
km$cluster


## Animation
library(animation)
windows()
km <- kmeans.ani(air_data,10)
