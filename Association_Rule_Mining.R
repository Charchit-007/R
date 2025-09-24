# Association rule mining 
#  -Aprori Algorithm

Training_data = list(c('A','B'),c('A','B','P','C'),c('A','P','C','D'),c('P','Q','R'),c('L','M','N'),c('A','B','C','D'),c('B','C','D'),c('L','M','N','A','B','C','D'))

apriori(Training_data)

Trans = as(Training_data,'transactions')

itemFrequencyPlot(Trans)

inspect(Trans)

rules = apriori(Trans,parameter = list(support = 0.3,confidence = 0.75))
inspect(rules)

#Top 5 rules based on confidence
sorted = sort(rules,5)
inspect(sorted)
ans = head(inspect(sorted),5)

data = read.csv('armdata.csv')

data$tid = as.factor(data$tid)
data$Items=as.character(data$Items)

Trans_List = split(data$Items,data$tid)

Trans2 = as(Trans_List,'transactions')

itemFrequencyPlot(Trans2)
inspect(Trans2)
rules = apriori(Trans2,parameter = list(support = 0.3,confidence = 0.75))
inspect(rules)
