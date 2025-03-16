# STAT 574 Midterm Exam Problem 4

#install.packages("caret")
library(readr)
library(caret)

hospital_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv",
header=T, sep=",")

# Split the data into 80% training and 20% testing sets. 

sample = sample(c(T,F), nrow(hospital_data), replace=T,
prob=c(0.8, 0.2))
train = hospital_data[sample,]
test = hospital_data[!sample,]

train_x = data.matrix(train[-6])
train_y = data.matrix(train[6])
test_x = data.matrix(test[-6])
test_y = data.matrix(test[6])

# Fitting the optimal KNN regressor. (k=6)

print(train(surgery_cost~age+BMI+ASA+surgery_duration_min,
data=train, method="knn"))

knn_reg = knnreg(train_x, train_y, k=6)

# Computing prediction accuracy on testing data.

pred_y = predict(knn_reg, test_x)

# Accuracy within 10%

accuracy10 = ifelse(abs(test_y-pred_y)<0.10*test_y, 1, 0)
print(mean(accuracy10))

# Accuracy within 15%

accuracy15 = ifelse(abs(test_y-pred_y)<0.15*test_y, 1, 0)
print(mean(accuracy15))

# Accuracy within 20%

accuracy20 = ifelse(abs(test_y-pred_y)<0.20*test_y, 1, 0)
print(mean(accuracy20))

# Plotting actual versus predicted values for testing data.

x = 1:length(test_y)
plot(x, test_y, type="l", lwd=2, col="magenta", main="KNN Regression", 
panel.first=grid())
lines(x, pred_y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("magenta","dodgerblue"))