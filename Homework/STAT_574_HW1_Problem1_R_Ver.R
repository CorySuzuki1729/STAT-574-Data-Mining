library(readr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(partykit)
library(CHAID)

hospital_data = read.csv(file="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv",
header=T, sep=",")

# (a) Splitting data into 80% training and 20% testing sets and building
# a regression tree on the training set using the RSS Splitting Criterion
# to model surgery cost.

set.seed(257496)
sample = sample(c(T,F), nrow(hospital_data),
replace=T, prob=c(0.8, 0.2))
train = hospital_data[sample,]
test = hospital_data[!sample,]

reg_tree_full = rpart(surgery_cost~gender+age+BMI+ASA
+ surgery_duration_min, data=train, method="anova", xval=10, cp=0)

printcp(reg_tree_full)

# Fitting regression tree with RSS Splitting and cost-complexity pruning

reg_tree_RSS = rpart(surgery_cost~gender+age+BMI+ASA
+ surgery_duration_min, data=train, method="anova",
cp=0.0041801)

#rpart.plot(reg_tree_RSS, type=3)

# Computing prediction accuracy for testing data within 10%, 15%, and
# 20%.

P_surgery_cost = predict(reg_tree_RSS, newdata=test)

# Accuracy within 10%

accuracy10 = ifelse(abs(test$surgery_cost-P_surgery_cost)<0.10*test$surgery_cost, 1, 0)
print(mean(accuracy10))

# Accuracy within 15%

accuracy15 = ifelse(abs(test$surgery_cost-P_surgery_cost)<0.15*test$surgery_cost, 1, 0)
print(mean(accuracy15))

# Accuracy within 20%

accuracy20 = ifelse(abs(test$surgery_cost-P_surgery_cost)<0.20*test$surgery_cost, 1, 0)
print(mean(accuracy20))

# Fitting regression tree with CHAID Splitting Criterion and cost-
# complexity pruning.

# Binning continuous predictor variables.

hospital_data = mutate(hospital_data, gender_cat = ntile(gender, 10),
age_cat = ntile(age, 10), BMI_cat = ntile(BMI, 10), ASA_cat = ntile(ASA, 10),
surgery_duration_min_cat = ntile(surgery_duration_min, 10),
surgery_cost_cat = ntile(surgery_cost, 10))

set.seed(233364)
sample = sample(c(T,F), nrow(hospital_data), replace=T,
prob=c(0.8, 0.2))
train = hospital_data[sample,]
test = hospital_data[!sample,]

reg_tree_CHAID = chaid(as.factor(surgery_cost_cat)~as.factor(gender_cat)+
as.factor(age_cat)+as.factor(BMI_cat)+as.factor(ASA_cat)+
as.factor(surgery_duration_min_cat), data=train,
control = chaid_control(maxheight=4))

plot(reg_tree_CHAID, type="simple")

# Computing prediction accuracy for testing data for CHAID regression
# tree

predclass = as.numeric(predict(reg_tree_CHAID, newdata=test))
test = cbind(test, predclass)

aggr_data = aggregate(train$surgery_cost, by=list(train$surgery_cost_cat),
FUN=mean)
aggr_data$predclass = aggr_data$Group.1
aggr_data$P_surgery_cost = aggr_data$x 
test = left_join(test, aggr_data, by='predclass')

# Accuracy within 10%

accuracy10 = ifelse(abs(test$surgery_cost-test$P_surgery_cost)<0.10*test$surgery_cost, 1, 0)
print(mean(accuracy10))

# Accuracy within 15%

accuracy15 = ifelse(abs(test$surgery_cost-test$P_surgery_cost)<0.15*test$surgery_cost, 1, 0)
print(mean(accuracy15))

# Accuracy within 20%

accuracy20 = ifelse(abs(test$surgery_cost-test$P_surgery_cost)<0.20*test$surgery_cost, 1, 0)
print(mean(accuracy20))