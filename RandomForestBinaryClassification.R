pneumonia.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv", 
header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(447558)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), 
replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]

#BUILDING RANDOM FOREST BINARY CLASSIFIER
library(randomForest)
rf.class<- randomForest(as.factor(pneumonia) ~ age + gender + tobacco_use	+ PM2_5,
data=train, ntree=150, mtry=4, maxnodes=30)

#DISPLAYING FEATURE IMPORTANCE
print(importance(rf.class,type=2)) 

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
predclass<- predict(rf.class, newdata=test)
test<- cbind(test,predclass)

accuracy<- c()
n<- nrow(test)
for (i in 1:n)
  accuracy[i]<- ifelse(test$pneumonia[i]==test$predclass[i],1,0)

print(accuracy<- mean(accuracy))


