rm(list = ls())
cat("\14")

##load data
crime_data<-read.csv("E:/itsstudytym/assignments/Cluster/crime_data C.csv")
str(crime_data)


#####heirarchical cluster #####


##normalization
crime<-scale(crime_data[,2:5])


#Computing the distance matrix
d<-dist(crime,method="euclidean")
fit<-hclust(d,method = "average")


# display dendogram
plot(fit)
group<-cutree(fit,k=4)


# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit,k=4,border="red")

#Attach the cluster numbers to Uni
clusters=data.frame('City'=crime_data[,1],'Cluster' =group)
clusters


##### k means cluster #####

## Elbow chart
wss<-c()
for(i in 2:15) wss[i]<-sum(kmeans(crime_data,centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "crime")


### Cluster algorithm building
km <- kmeans(crime_data,7) 
km$centers
km$cluster


## Animation
library(animation)
windows()
km <- kmeans.ani(crime_data,7)

