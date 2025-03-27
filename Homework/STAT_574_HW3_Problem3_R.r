# STAT 574 HW3 Problem 3

#install.packages("caTools")
#install.packages("neuralnet")
library(readr)
library(dplyr)
library(caTools)
library(neuralnet)

hospital_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv",
header=T, sep=",") 

hospital_data$gender = ifelse(hospital_data$gender=='M', 1, 0)

# Scaling the variables to fall within [0,1]. 

scale01 = function(x) {
    (x-min(x))/(max(x)-min(x))
}

hospital_data = hospital_data %>% mutate_all(scale01)

# Splitting the data into 80% training and 20% testing sets. 

set.seed(566409)

sample = sample(c(T,F), nrow(hospital_data), replace=T,
prob=c(0.8, 0.2))
train = hospital_data[sample,]
test = hospital_data[!sample,]

test_x = data.matrix(test[2:6])
test_y = data.matrix(test[7])

# Fitting an ANN for regression. 

ann_reg = neuralnet(surgery_cost~gender+age+BMI+ASA
+surgery_duration_min, data=train, hidden=3, act.fct="logistic",
stepmax=1e7)

# Plotting the diagram. 

plot(ann_reg)

# Computing prediction accuracy for testing data. 

pred_y = predict(ann_reg, test_x)

# Accuracy within 10%

accuracy10 = ifelse(abs(test_y-pred_y)<0.10*test_y, 1, 0)

# Accuracy within 15%

accuracy15 = ifelse(abs(test_y-pred_y)<0.15*test_y, 1, 0)

# Accuracy within 20%

accuracy20 = ifelse(abs(test_y-pred_y)<0.20*test_y, 1, 0)

print('Prediction Accuracy for Logistic Activation Function')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

##############################################################################

ann_reg_tanh = neuralnet(surgery_cost~gender+age+BMI+ASA
+surgery_duration_min, data=train, hidden=3, act.fct="logistic",
stepmax=1e7)

# Plotting the diagram. 

plot(ann_reg_tanh)

# Computing the prediction accuracy for testing data. 

y_pred_tanh = predict(ann_reg_tanh, test_x)

# Accuracy within 10%

accuracy10_tanh = ifelse(abs(test_y-y_pred_tanh)<0.10*test_y, 1, 0)

# Accuracy within 15%

accuracy15_tanh = ifelse(abs(test_y-y_pred_tanh)<0.15*test_y, 1, 0)

# Accuracy within 20%

accuracy20_tanh = ifelse(abs(test_y-y_pred_tanh)<0.20*test_y, 1, 0)

print("Prediction accuracy for Tanh activation function")
print(paste('within 10%:', round(mean(accuracy10_tanh),4)))
print(paste('within 15%:', round(mean(accuracy15_tanh),4)))
print(paste('within 20%:', round(mean(accuracy20_tanh),4)))

# Plotting the actual vs predicted values for testing data. 

x<- 1:length(test)
plot(x, test, type="l", lwd=2, col="magenta", main="ANN Regression with 
Tanh Activation Function", panel.first=grid())
lines(x, y_pred_tanh, lwd=2, col="dodgerblue")
points(x,test_y, pch=16, col="purple")
points(x, y_pred_tanh, pch=16, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("purple","dodgerblue"))