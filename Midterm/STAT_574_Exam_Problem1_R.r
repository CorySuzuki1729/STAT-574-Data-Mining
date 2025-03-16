# STAT 574 Midterm Exam Problem 1

install.packages("e1071")
library(readr)
library(e1071)

hospital_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv",
header=T, sep=",")

head(hospital_data)

# Splitting the data into 80% training and 20% testing sets.

set.seed(562323)
sample = sample(c(T,F), nrow(hospital_data),
replace=T, prob=c(0.8, 0.2))
train = hospital_data[sample,]
test = hospital_data[!sample,]

# Fitting support vector regression with linear kernel.

svr_linear = svm(surgery_cost~age+BMI+ASA+surgery_duration_min,
data=train, kernel="linear")

# Computing prediction accuracy on testing data.

pred_y = predict(svr_linear,newdata=test)

# Accuracy within 10%

accuracy10 = ifelse(abs(test$surgery_cost-pred_y)<0.10*test$surgery_cost, 1, 0)

# Accuracy within 15%

accuracy15 = ifelse(abs(test$surgery_cost-pred_y)<0.15*test$surgery_cost, 1, 0)

# Accuracy within 20%

accuracy20 = ifelse(abs(test$surgery_cost-pred_y)<0.20*test$surgery_cost, 1, 0)

print('Linear Kernel')
print(paste('within 10%:', round(mean(accuracy10),5)))
print(paste('within 15%:', round(mean(accuracy15),5)))
print(paste('within 20%:', round(mean(accuracy20),5)))

# Fitting SVR with Polynomial Kernel. 

svr_poly = svm(surgery_cost~age+BMI+ASA+surgery_duration_min,
data=train, kernel="poly")

# Computing prediction accuracy for testing data.

pred_y = predict(svr_poly, newdata=test)

# Accuracy within 10%

accuracy10_poly = ifelse(abs(test$surgery_cost-pred_y)<0.10*test$surgery_cost, 1, 0)

# Accuracy within 15%

accuracy15_poly = ifelse(abs(test$surgery_cost-pred_y)<0.15*test$surgery_cost, 1, 0)

# Accuracy within 20%

accuracy20_poly = ifelse(abs(test$surgery_cost-pred_y)<0.20*test$surgery_cost, 1, 0)

print('Polynomial Kernel')
print(paste('within 10%:', round(mean(accuracy10_poly),5)))
print(paste('within 15%:', round(mean(accuracy15_poly),5)))
print(paste('within 20%:', round(mean(accuracy20_poly),5)))

# Fitting SVR with Radial kernel. 

svr_radial = svm(surgery_cost~age+BMI+ASA+surgery_duration_min,
data=train, kernel="radial")

# Computing prediction accuracy on testing data. 

pred_y = predict(svr_radial, newdata=test)

# Accuracy within 10%

accuracy10_radial = ifelse(abs(test$surgery_cost-pred_y)<0.10*test$surgery_cost, 1, 0)

# Accuracy within 15%

accuracy15_radial = ifelse(abs(test$surgery_cost-pred_y)<0.15*test$surgery_cost, 1, 0)

# Accuracy within 20%

accuracy20_radial = ifelse(abs(test$surgery_cost-pred_y)<0.20*test$surgery_cost, 1, 0)

print('Radial Kernel')
print(paste('within 10%:', round(mean(accuracy10_radial),5)))
print(paste('within 15%:', round(mean(accuracy15_radial),5)))
print(paste('within 20%:', round(mean(accuracy20_radial),5)))

# Fitting SVR with sigmoid kernel. 

svr_sig = svm(surgery_cost~age+BMI+ASA+surgery_duration_min,
data=train, kernel="sigmoid")

# Computing prediction accuracy on testing data. 

pred_y = predict(svr_sig, newdata=test)

# Accuracy within 10%

accuracy10_sig = ifelse(abs(test$surgery_cost-pred_y)<0.10*test$surgery_cost, 1, 0)

# Accuracy within 15%

accuracy15_sig = ifelse(abs(test$surgery_cost-pred_y)<0.15*test$surgery_cost, 1, 0)

# Accuracy within 20%

accuracy20_sig = ifelse(abs(test$surgery_cost-pred_y)<0.20*test$surgery_cost, 1, 0)

print('Sigmoid Kernel')
print(paste('within 10%:', round(mean(accuracy10_sig),5)))
print(paste('within 15%:', round(mean(accuracy15_sig),5)))
print(paste('within 20%:', round(mean(accuracy20_sig),5)))