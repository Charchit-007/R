# random forest -> bootstrap sampling #library(randomForest)
# sqrt(No of attributes) == Attribute to consider in tree]
data = iris
set.seed(123)
sample_index = sample(1:nrow(iris),0.9 * nrow(iris))  #90% train

train_data = iris[sample_index, ]
test_data = iris[-sample_index, ]
model = randomForest(Species ~ ., data = train_data ,ntree = 100)
print(model)

predictions = predict(model,test_data)
table(predictions,test_data$Species)
/******************************************************************************/
data2 = read.csv('placement.csv')
set.seed(132)
sample_index2 = sample(1:nrow(data2),0.75 * nrow(data2))

# convert categorical variables
data2$InternshipExperience <- ifelse(data2$InternshipExperience == "Yes", 1, 0)
data2$ExtraCurricular = ifelse(data2$ExtraCurricular == "Yes", 1, 0)
data2$CertificationCourse = ifelse(data2$CertificationCourse == "Yes", 1, 0)
data2$Placement = as.factor(data2$Placement)


train_data2 = data2[sample_index2, ]
test_data2 = data2[-sample_index2, ]
model = randomForest(Placement ~ ., data= train_data2, ntree = 100)
print(model)

predictions = predict(model,test_data2)
table(predictions,test_data2$Placement)
tree1 = getTree(model,k=1,labelVar = TRUE)
confusionMatrix(predictions, test_data2$Placement)  #library(caret)

model = randomForest(
  Placement ~ .,
  data = train_data2,
  ntree = 500,
  mtry = 2,
  importance = TRUE,
  proximity = TRUE,
  sampsize = c(50,50),
  replace = TRUE,
  nodesize = 5,
  maxnodes = 30,
  keep.forest = TRUE,
  do.trace = 100
)
