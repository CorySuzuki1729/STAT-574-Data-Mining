# STAT 574 Midterm Exam Problem 3

library(readr)
library(e1071)

concussion_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv",
header=T, sep=",")

# Splitting the data into 80% training and 20% testing sets. 

set.seed(562310)
sample = sample(c(T,F), nrow(concussion_data), replace=T,
prob=c(0.8, 0.2))
train = concussion_data[sample,]
test = concussion_data[!sample,]

# Fitting a multinomial SVC with linear kernel. 

svc_multi_lin = svm(as.factor(concussion)~age+nyearsplaying
+position+prevconc, data=train, kernel="linear")

# Computing prediction accuracy for testing data. 

pred_y = predict(svc_multi_lin, newdata=test)
len = nrow(test)
test = cbind(test, pred_y)
match = c()

for (i in 1:len) {
    match[i] =ifelse(test$concussion[i] == test$pred_y[i], 1, 0)
}

print(paste("Linear Accuracy:", round(mean(match), 5)))

# Fitting a multinomial SVC with polynomial kernel.

svc_multi_poly = svm(as.factor(concussion)~age+nyearsplaying
+position+prevconc, data=train, kernel="polynomial")

# Computing prediction accuracy on testing data.

pred_y_poly = predict(svc_multi_poly, newdata=test)
len = nrow(test)
test = cbind(test, pred_y_poly)
match2 = c()

for (i in 1:len) {
    match2[i] =ifelse(test$concussion[i] == test$pred_y_poly[i], 1, 0)
}

print(paste("Polynomial Accuracy:", round(mean(match2), 5)))

# Fitting a multinomial SVC with radial kernel.

svc_multi_rad = svm(as.factor(concussion)~age+nyearsplaying
+position+prevconc, data=train, kernel="radial")

# Computing prediction accuracy on testing data. 

pred_y_rad = predict(svc_multi_rad, newdata=test)
len = nrow(test)
test = cbind(test, pred_y_rad)
match3 = c()

for (i in 1:len) {
    match3[i] =ifelse(test$concussion[i] == test$pred_y_rad[i], 1, 0)
}

print(paste("Radial Accuracy:", round(mean(match3), 5)))

# Fitting a multinomial SVC with sigmoid. 

svc_multi_sig = svm(as.factor(concussion)~age+nyearsplaying
+position+prevconc, data=train, kernel="sigmoid")

# Computing prediction accuracy on testing set. 

pred_y_sig = predict(svc_multi_sig, newdata=test)
len = nrow(test)
test = cbind(test, pred_y_sig)
match4 = c()

for (i in 1:len) {
    match4[i] =ifelse(test$concussion[i] == test$pred_y_sig[i], 1, 0)
}

print(paste("Sigmoid Accuracy:", round(mean(match4), 5)))