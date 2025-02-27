install.packages("randomForest")
library(readr)
library(randomForest)

hospital_data = read.csv(file="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv",
header=T, sep=",")

# Splitting data into 80% training and 20% testing sets.

set.seed(364323)
sample = sample(c(T,F), nrow(hospital_data),
replace=T, prob=c(0.8, 0.2))
train = hospital_data[sample,]
test = hospital_data[!sample,]

# Building random forest regression.

rf_reg_hosp = randomForest(surgery_cost~age+BMI+ASA
+surgery_duration_min, data=train, ntree=150, mtry=5,
maxnodes=30)

# Displaying feature importance. 

print(importance(rf_reg_hosp, type=2))

# Computing prediction accuracy for testing set.

P_surgery_cost = predict(rf_reg_hosp, newdata=test)

# Accuracy within 10%

accuracy10 = ifelse(abs(test$surgery_cost-P_surgery_cost)<0.10*test$surgery_cost, 1, 0)
print(accuracy10 <- mean(accuracy10))

# Accuracy within 15%

accuracy15 = ifelse(abs(test$surgery_cost-P_surgery_cost)<0.15*test$surgery_cost, 1, 0)
print(accuracy15 <- mean(accuracy15))

# Accuracy within 20%

accuracy20 = ifelse(abs(test$surgery_cost-P_surgery_cost)<0.20*test$surgery_cost, 1, 0)
print(accuracy20 <- mean(accuracy20))