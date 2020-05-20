mydata1<-read.csv("C:/Users/sonih/Downloads/Universities.csv")

###########normalization
mydata<-scale(mydata1[,2:7])

d<-dist(mydata, method = "euclidean") #Computing the distance matrix
fit<-hclust(d, method="average") # Building the algorithm # try with 'centroid'


plot(fit) # display dendogram
groups<-cutree(fit, k=4) # cut tree into 4 clusters


# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit,k=5,border="red")


#Attach the cluster numbers to Uni
clusters=data.frame('Uni'=mydata1[,1],'Cluster' =groups)
clusters
