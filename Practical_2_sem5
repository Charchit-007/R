#Prac 2

data =read.csv('campus_placement_dataset - campus_placement_dataset.csv')
summary(data)

#Convert categorial values
data$InternshipExperience = ifelse(data$InternshipExperience == "Yes",1,0)
data$ExtraCurricular = ifelse(data$ExtraCurricular == "Yes" ,1,0)
data$CertificationCourse = ifelse(data$CertificationCourse == "Yes", 1 ,0)
data$Placement = as.factor(data$Placement)

train_data= data[1:180, ]
test_data = data[181:200, ]
test_labels = test_data$Placement
test_data = test_data[, -8] # it will not take 8th column (placement)

testtree_model = rpart(Placement ~ ., data = train_data, method ="class")
rpart.plot(testtree_model, type = 2, extra = 104, fallen.leaves = TRUE,main = "Decision treee for placement" )
result = predict(testtree_model,test_data,type='class')
result
table(result,test_labels)

confusionMatrix(result,test_labels) # to check accuracy

#Filter column
information_gain(Placement ~.,data = data)

# onlu considering columns Internship_exp , Project_done , Extracurricular
testtree_model2 = rpart(Placement ~  InternshipExperience+ExtraCurricular+ProjectsDone+CommunicationSkills , data = train_data , method = "class")
rpart.plot(testtree_model2, type = 2, extra = 104, fallen.leaves = TRUE,main = "Decision treee for placement" )
result2 = predict(testtree_model2,test_data,type='class')
result2
table(result,test_labels)
confusionMatrix(result2,test_labels)


model_tree = tree(Placement ~., data = train_data)
pred_tree = predict(model_tree,test_data,type="class")
table(Predicted = pred_tree,Actual = test_labels)

plot(model_tree)
text(model_tree,pretty = 0)


