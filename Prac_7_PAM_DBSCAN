# PAM (Partitioning Around Medoids) ALgo
library(cluster)
data = iris
data = data[,-5]
summary(data)
model = pam(data,k=3) 
model$clusinfo
model$cluster
model$clustering # show each element belong to which cluster
model$medoids
data$clusters=model$clustering #Adding the cluster col in data
data_filtered = data %>% filter(clusters == 3) 
clusplot(model)
#Another way using normal plot funciton
plot(data$Sepal.Length,data$Petal.Length,col=model$clustering)

#DBSCAN
df = iris
df = df[,-5]
model = dbscan(df,eps=0.5,minPts = 5)
model
model$cluster




data_gc = read.csv('tyit practical sept3.csv')
model_gc= dbscan(data_gc,eps=3,minPts = 3)
data_gc$clusters=model_gc$cluster
model_gc
plot(data_gc$x,data_gc$y,col = data_gc$cluster)


