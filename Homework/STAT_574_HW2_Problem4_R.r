install.packages("xgboost")
library(xgboost)

hospital_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv",
header=T, sep=",")

# Splitting the data into 80% training and 20% testing sets.

set.seed(698498)
sample = sample(c(T,F), nrow(hospital_data), replace=T, prob=c(0.8, 0.2))
train = hospital_data[sample,]
test = hospital_data[!sample,]

train_x = data.matrix(train[-6])
train_y = data.matrix(train[6])
test_x = data.matrix(test[-6])
test_y = data.matrix(test[6])

# Fitting Extreme Gradient boosted Regression Tree

xgb_reg = xgboost(data=train_x, label=train_y,
max.depth=6, eta=0.01, subsample=0.8, colsample_bytree=0.5,
nrounds=1000, objective="reg:linear")

# Displaying Feature Importance

print(xgb.importance(colnames(train_x), model=xgb_reg))

# Computing prediction accuracy for testing data

pred_y = predict(xgb_reg, test_x)

# Accuracy within 10%

accuracy10 = ifelse(abs(test_y-pred_y)<0.10*test_y, 1, 0)
print(mean(accuracy10))

# Accuracy within 15%

accuracy15 = ifelse(abs(test_y-pred_y)<0.15*test_y, 1, 0)
print(mean(accuracy15))

# Accuracy within 20%

accuracy20 = ifelse(abs(test_y-pred_y)<0.20*test_y, 1, 0)
print(mean(accuracy20))