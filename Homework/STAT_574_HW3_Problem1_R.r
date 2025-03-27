# STAT 574 HW3 Problem 1

library(readr)
library(e1071)

card_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv",
header=T, sep=",")

# Splitting the data into 80% training and 20% testing sets. 

set.seed(111009)
sample = sample(c(T,F), nrow(card_data), replace=T, prob=c(0.8, 0.2))
train = card_data[sample,]
test = card_data[!sample,]

# Fitting binary Naive Bayes classifier. 

nb_binary = naiveBayes(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train)

# Computing prediction accuracy for testing data. 

y_pred = predict(nb_binary, newdata=test)
len = nrow(test)
test = cbind(test, y_pred)
match = c()

for (i in 1:len) {
    match[i] = ifelse(test$fraud[i]==test$y_pred[i], 1, 0)
}

print(paste('Accuracy:', round(mean(match)*100, 2), '%'))
