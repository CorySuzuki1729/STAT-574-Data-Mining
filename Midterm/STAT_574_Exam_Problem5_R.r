# STAT 574 Midterm Exam Problem 5

library(readr)
library(caret)

card_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv",
header=T, sep=",")

# Splitting the data into 80% training and 20% testing sets. 

set.seed(424903)
sample = sample(c(T,F), nrow(card_data), replace=T,
prob=c(0.8, 0.2))

train = card_data[sample,]
test = card_data[!sample,]

train_x = data.matrix(train[-7])
train_y = data.matrix(train[7])
test_x = data.matrix(test[-7])
test_y = data.matrix(test[7])

# Fitting optimal KNN Binary Classifier (k=9)

print(train(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, method="knn"))

knn_bin = knnreg(train_x, train_y, k=9)

# Computing prediction accuracy on testing data. 

pred_prob = predict(knn_bin, test_x)

len = length(pred_prob)
pred_y = c()
match = c()

for (i in 1:len) {
    pred_y[i] = ifelse(pred_prob[i] >= 0.5, 1, 0)
    match[i] = ifelse(test_y[i]==pred_y[i], 1, 0)
}

accuracy = mean(match)
print(paste("accuracy=", round(accuracy, 5)))
