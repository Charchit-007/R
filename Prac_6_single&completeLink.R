# Assigment - 20-08-2025
# Q1
################################ Single Link #######################################
data = read.csv('Mall_Customers - Mall_Customers.csv')

data = data[,c(4,5)] #gets annnual income by age

normalized = function(x){
  return (((x-min(x)))/(max(x)-min(x)))
}
data$Annual_Income = normalized(data$Annual_Income)
data$Spending_Score = normalized(data$Spending_Score)

dist_matrix = dist(data,method="euclidean")

Model = hclust(dist_matrix,method = 'single')

clusters = cutree(Model,5)

plot(Model,main='SingleLinkAlgorithm')

dg = as.dendrogram(Model)
plot(dg,main='SingleLinkAlgorithm')
ten = cutree(Model,10)
ColorDendrogram(Model,ten,branchlength = 10)

################################ Complete Link #######################################

data_mall = read.csv('Mall_Customers - Mall_Customers.csv')

data = data_mall[,c(4,5)] #gets annnual income by age

normalized = function(x){
  return (((x-min(x)))/(max(x)-min(x)))
}
data$Annual_Income = normalized(data$Annual_Income)
data$Spending_Score = normalized(data$Spending_Score)

dist_matrix = dist(data,method="euclidean")

Model = hclust(dist_matrix,method = 'complete')

clusters = cutree(Model,5)

plot(Model,main='CompleteLinkAlgorithm')

dg = as.dendrogram(Model)
plot(dg,main='CompleteLinkAlgorithm')
ten = cutree(Model,10)
ColorDendrogram(Model,ten,branchlength = 10)



data_mall $clusters = clusters # add columns clusters in data
data_mall[data_mall$clusters== 4,] # filter clusters == 4 as per the question



#**********************************************  Q 2  ******************************************************#
cities_data = read.csv('worldcities - worldcities.csv')

d_ind = filter(cities_data,country == "India")
d_jap = filter(cities_data,country == "Japan")
test_data = rbind(d_ind,d_jap)

clust_data = test_data[,c(3,4)]

normalized = function(x){
  return (((x-min(x)))/(max(x)-min(x)))
}
clust_data$lat = normalized(clust_data$lat)
clust_data$lng = normalized(clust_data$lng)

cities_matrix = dist(clust_data,method="euclidean")

Model = hclust(cities_matrix,method = 'single')

clusters = cutree(Model,5)
clusters

test_data$clusters = clusters

plot(Model,main='singleLinkAlgorithm')

dg = as.dendrogram(Model)
plot(dg,main='singleLinkAlgorithm')
five = cutree(Model,5)
ColorDendrogram(Model,five,branchlength = 10)


min_test_data = test_data[c(1,2,3,4,5,8,12)] # selecting columns 
min_test_data[min_test_data$clusters!=1 & min_test_data$clusters!=2,]
min_test_data[min_test_data$clusters==3,]
