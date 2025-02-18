library(readr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(partykit)
library(CHAID)

card_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv",
header=T, sep=",")

# Splitting data into 80% training and 20% testing sets.

set.seed(122470)
sample = sample(c(T,F), nrow(card_data),
replace=T, prob=c(0.8, 0.2))
train = card_data[sample,]
test = card_data[!sample,]

# Fitting pruned binary tree with Gini Splitting Criterion.

tree_gini = rpart(fraud~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, method="class", parms=list(split="Gini"),
maxdepth=7)

#rpart.plot(tree_gini, type=3)

pred_values = predict(tree_gini, test)
test = cbind(test, pred_values)

tp = c()
fp = c()
tn = c()
fn = c()

total = nrow(test)
for (i in 1:total) {
    tp[i] = ifelse(test$"1"[i]>0.5 & test$fraud[i]==1,1,0)
    fp[i] = ifelse(test$"1"[i]>0.5 & test$fraud[i]==0,1,0)
    tn[i] = ifelse(test$"1"[i]>0.5 & test$fraud[i]==0,1,0)
    fn[i] = ifelse(test$"1"[i]>0.5 & test$fraud[i]==1,1,0)
}

print(tp <- sum(tp))
print(fp <- sum(fp))
print(tn <- sum(tn))
print(fn <- sum(fn))
print(total)

print(accuracy <- (tp+tn)/total)
print(misclassrate <- (fp+fn)/total)
print(sensitivity <- tp/(tp+fn))
print(FNR <- fn/(tp+fn))
print(specificity <- tn/(fp+tn))
print(FPR <- fp/(fp+tn))
print(precision <- tp/(fp+fp))
print(NPV <- tn/(fn+tn))
print(F1score <- 2*tp/(2*tp+fn+fp))