# STAT 574 HW3 Problem 5

library(readr)
library(dplyr)
library(neuralnet)

concussion_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv",
header=T, sep=",")

concussion_data$position = ifelse(concussion_data$position=='Offensive Lineman', 1,
ifelse(concussion_data$position=='Cornerback', 2,
ifelse(concussion_data$position=='Wide Receiver', 3, 
ifelse(concussion_data$position=='Runningback',4,5))))

concussion_data$concussion = ifelse(concussion_data$concussion=='mild',1,
ifelse(concussion_data$concussion=='moderate', 2, 3))

scale01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

concussion_data = concussion_data %>% mutate_all(scale01)

# Splitting data into 80% training and 20% testing sets. 

set.seed(273194)
sample = sample(c(T,F), nrow(concussion_data), replace=T,
prob=c(0.8, 0.2))
train = concussion_data[sample,]
test = concussion_data[!sample,]

train_x = data.matrix(train[-5])
train_y = data.matrix(train[5])
test_x = data.matrix(test[-5])
test_y = data.matrix(test[5])

# Fitting an ANN Multinomial Classifier with logistic activation function. 

ann_log_multi = neuralnet(as.factor(concussion)~age+nyearsplaying+position
+prevconc, data=train, hidden=3, act.fct="logistic", stepmax=1e7)

plot(ann_log_multi)

# Computing prediction accuracy for testing data. 

pred_prob = predict(ann_log_multi, test_x)
pred_prob = as.data.frame(pred_prob)

colnames(pred_prob) = c(0, 0.5, 1)

pred_class = apply(pred_prob, 1, function(x) colnames(pred_prob)[which.max(x)])

match = c()
for (i in 1:length(test_y)) {
    match[i] = ifelse(pred_class[i]==as.character(test_y[i]), 1, 0)
}

print(paste("Accuracy:", round(mean(match), 4)))

#######################################################################

# Fitting an ANN Multinomial Classifier with tanh activation function. 

ann_tanh_multi = neuralnet(as.factor(concussion)~age+nyearsplaying+position
+prevconc, data=train, hidden=3, act.fct="tanh", stepmax=1e7)

plot(ann_tanh_multi)

# Computing prediction accuracy for testing data. 

pred_prob = predict(ann_tanh_multi, test_x)
pred_prob = as.data.frame(pred_prob)

colnames(pred_prob) = c(0, 0.5, 1)

pred_class = apply(pred_prob, 1, function(x) colnames(pred_prob)[which.max(x)])

match = c()
for (i in 1:length(test_y)) {
    match[i] = ifelse(pred_class[i]==as.character(test_y[i]), 1, 0)
}

print(paste("Accuracy:", round(mean(match), 4)))