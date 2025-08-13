data = iris
data = data[,-5]
model = kmeans(data,centers = 3 , nstart = 27) # nstart -> how many different cases to consider should be between 20 to 30
model$centers
model$cluster # each data point which belongs to which cluster\
plot(data$Sepal.Length,data$Sepal.Width,col= model$cluster,pch=19,xlab = "Sepal Length", ylab = "Sepal Width")



data = USArrests
normalized = function(x){
  return (((x-min(x)))/(max(x)-min(x)))
}
data$Murder = normalized(data$Murder)
data$Assault = normalized(data$Assault)
data$UrbanPop = normalized(data$UrbanPop)
data$Rape = normalized(data$Rape) 
model = kmeans(data,centers = 3 , nstart = 27)
model$centers
data$group = model$cluster
#x = data.frame(model$cluster)
#Cities with Low crime overall.
best_city = data %>% filter (data$group == 2)
#Cities with High murder/assault/rape but lower urban population.
worst_city = data %>% filter (data$group == 3)


data = read.csv('exam.csv')
model = kmeans(data,centers = 4 , nstart = 27)
model$centers
model$cluster
model$size
