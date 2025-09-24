#clustering Algo -> Average link algorithm
#Package - 
 # 1.cluster
#  2.sparcl

# Apply above algo

data = read.csv('AvgLinkAlgo.csv')

adj_matrix = as.dist(data)
model = hclust(adj_matrix,method='average')
#heirarchy cluster - hcluster

plot(model,main='AverageLinkAlgorithm')

dg = as.dendrogram(model)
plot(dg,main='AvgerageLinkAlgorithm')
model$merge
#gives the formation of cluster
#-1  -3  cluster formation (A and C)
#-2  -5  cluster formation ( B and E)
#-4   2  cluster formation (D with cluster 2 that is {B,E})
# 1   3  cluster formation ( 1st({A,C}) and 3rd{D,B,E} cluster)


# - represnt object , +ve values represnt cluster

#if want only two cluster
two = cutree(model,2)
three = cutree(model,3)
five = cutree(model,5)

ColorDendrogram(model,three,branchlength = 4)

#with iris data set

Bigdata = iris
Bigdata = Bigdata[,-5] #REMOVE 5TH COL
Big_adj_matrix = dist(Bigdata,method='euclidean')
model = hclust(Big_adj_matrix,method='average')

dg = as.dendrogram(model)
plot(dg,main='AvgLinkAlgo')
ten = cutree(model,10)
ColorDendrogram(model,ten,branchlength = 10)
