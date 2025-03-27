# STAT 574 HW3 Problem 4

library(readr)
library(dplyr)
library(caTools)
library(neuralnet)

card_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv",
header=T, sep=",")

set.seed(111082)

sample = sample.split(card_data, SplitRatio=0.8)
train = subset(card_data, sample==T)
test = subset(card_data, sample==F)

train_x = data.matrix(train[-4])
train_y = data.matrix(train[4])
test_x = data.matrix(test[-4])
test_y = data.matrix(test[4])

# Fitting an ANN for binary classification with logistic activation function.

ann_logistic = neuralnet(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, hidden=3, act.fct="logistic", stepmax=1e7)

plot(ann_logistic)

# Computing prediction accuracy for testing data. 

pred_prob = predict(ann_logistic, test_x)[,1]
pred_y = c()
match = c()
for (i in 1:length(test_y)) {
    pred_y[i] = ifelse(pred_prob[i]>=0.5,1,0)
    match[i] = ifelse(test_y[i]==pred_y[i],1,0)
}

print(paste("Accuracy:", round(mean(match), 4)))

######################################################################

# Fitting an ANN binary classifier using the Tanh activation function.

ann_tanh_bin = neuralnet(as.factor(fraud)~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, hidden=3, act.fct="tanh", stepmax=1e7)

plot(ann_tanh_bin)

# Computing prediction accuracy for testing data. 

pred_prob = predict(ann_tanh_bin, test_x)[,1]
pred_y = c()
match = c()
for (i in 1:length(test_y)){
    pred_y[i] = ifelse(pred_prob[i]>=0.5,1,0)
    match[i] = ifelse(test_y[i]==pred_y[i],1,0)
}

print(paste("Accuracy:", round(mean(match), 4)))