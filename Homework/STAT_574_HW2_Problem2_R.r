library(readr)
library(randomForest)

card_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv",
header=T, sep=",")

# Splitting the data into 80% training and 20% testing sets.

set.seed(474123)
sample = sample(c(T,F), nrow(card_data),
replace=T, prob=c(0.8, 0.2))
train = card_data[sample,]
test = card_data[!sample,]

# Building random forest binary classifier. 

rf_bin_cls = randomForest(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, ntree=150, mtry=4, maxnodes=30)

# Displaying feature importance. 

print(importance(rf_bin_cls, type=2))

# Computing prediction accuracy for testing data. 

predclass = predict(rf_bin_cls, newdata=test)
test = cbind(test, predclass)

accuracy = c()
n = nrow(test)
for (i in 1:n) {
    accuracy[i] = ifelse(test$fraud[i] == test$predclass[i], 1, 0)
}
print(accuracy <- mean(accuracy))