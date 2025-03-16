# STAT 574 Midterm Exam Problem 2

library(readr)
library(e1071)

card_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv",
header=T, sep=",")

# Splitting data into 80% training and 20% testing sets. 

set.seed(909808)
sample = sample(c(T,F), nrow(card_data), replace=T,
prob=c(0.8, 0.2))
train = card_data[sample,]
test = card_data[!sample,]

# Fitting binary SVC with linear kernel. 

svc_bin = svm(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, kernel="linear")

# Computing prediction accuracy for testing data. 

pred_y = predict(svc_bin, newdata=test)
len = nrow(test)
test = cbind(test, pred_y)
match = c()

for (i in 1:len) {
    match[i] =ifelse(test$fraud[i] == test$pred_y[i], 1, 0)
}

print(paste("Linear Accuracy:", round(mean(match), 5)))

# Fitting binary SVC with polynomial kernel.

svc_binpoly = svm(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, kernel="polynomial")

# Computing prediction accuracy using testing set. 

pred_y_poly = predict(svc_binpoly, newdata=test)
test = cbind(test, pred_y_poly)
match2 = c()

for (i in 1:nrow(test)) {
    match2[i] = ifelse(test$fraud[i] == test$pred_y_poly[i], 1, 0)
}

print(paste("Polynomial Accuracy:", round(mean(match2), 5)))

# Fitting binary SVC with radial kernel. 

svc_binrad = svm(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, kernel="radial")

# Computing prediction accuracy from testing data. 

pred_y_rad = predict(svc_binrad, newdata=test)
test = cbind(test, pred_y_rad)
match3 = c()

for (i in 1:nrow(test)) {
    match3[i] = ifelse(test$fraud[i] == test$pred_y_rad[i], 1, 0)
}

print(paste("Radial Accuracy:", round(mean(match3), 5)))

# Fitting binary SVC with sigmoid kernel. 

svc_binsig = svm(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, kernel="sigmoid")

# Computing prediction accuracy from testing data. 

pred_y_sig = predict(svc_binsig, newdata=test)
test = cbind(test, pred_y_sig)
match4 = c()

for (i in 1:nrow(test)){
    match4[i] = ifelse(test$fraud[i] == test$pred_y_sig[i], 1, 0)
}

print(paste("Sigmoid accuracy:", round(mean(match4), 5)))