# STAT 574 Midterm Exam Problem 6

library(readr)
library(caret)

concussion_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv",
header=T, sep=",")

# Splitting the data into 80% training and 20% testing sets. 

set.seed(294908)
sample = sample(c(T,F), nrow(concussion_data), replace=T,
prob=c(0.8, 0.2))

train = concussion_data[sample,]
test = concussion_data[!sample,]

train_x = data.matrix(train[-4])
train_y = data.matrix(train[4])
test_x = data.matrix(test[-4])
test_y = data.matrix(test[4])

# Fitting the optimal KNN Multinomial Classifier (k=11)

print(train(as.factor(concussion)~age+nyearsplaying
+position+prevconc, data=train, method="knn"))

knn_multi = knnreg(train_x, train_y, k=11)

# Computing prediction accuracy for testing data. 

pred_y = round(predict(knn_multi, test_x), digits=0)
print(paste("accuracy=", round(1-mean(test_y!=pred_y),digits=5)))