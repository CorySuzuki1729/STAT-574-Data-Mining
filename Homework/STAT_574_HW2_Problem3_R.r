# STAT 574 HW2 Problem 3

library(readr)
library(randomForest)

concussion_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv",
header=T, sep=",")

# Splitting data into 80% training and 20% testing sets.

set.seed(333528)
sample = sample(c(T,F), nrow(concussion_data), replace=T,
prob=c(0.8, 0.2))
train = concussion_data[sample,]
test = concussion_data[!sample,]

# Building a multinomial random forest classifier.

rf_multi_class = randomForest(as.factor(concussion)~age+nyearsplaying
+position+prevconc, data=train, ntree=150, mtry=4, maxnodes=30)

# Displaying feature importance. 

print(importance(rf_multi_class, type=2))

# Computing prediction accuracy from testing data.

predclass = predict(rf_multi_class, newdata=test)
test = cbind(test, predclass)

accuracy = c()
for (i in 1:nrow(test)) {
    accuracy[i] = ifelse(test$concussion[i] == test$predclass[i], 1, 0)
}

print(accuracy <- mean(accuracy))