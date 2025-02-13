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

# Computing prediction accuracy for testing data for Gini Tree.

pred_values = predict(tree_gini, test)
test = cbind(test, pred_values)

tp = matrix(NA, nrow=nrow(test), ncol=99)
tn = matrix(NA, nrow=nrow(test), ncol=99)

for (i in 1:99) {
    tp[,i] = ifelse(test$fraud=="1" & test$"1">0.01*i,1,0)
    tn[,i] = ifelse(test$fraud=="0" & test$"1"<=0.01*i,1,0)
}

trueclassrate = matrix(NA, nrow=99, ncol=2)
for (i in 1:99) {
    trueclassrate[i,1] = 0.01*i
    trueclassrate[i,2] = sum(tp[,i]+tn[,i])/nrow(test)
}

print(trueclassrate[which(trueclassrate[,2]==max(trueclassrate[,2])),])

# Fitting pruned binary tree with entropy splitting

tree_entropy = rpart(fraud~distance_from_home+distance_from_last_transaction
+ratio_to_median_purchase_price+repeat_retailer+used_chip+used_pin_number
+online_order, data=train, method="class", parms=list(split="Gini"),
maxdepth=7)

#rpart.plot(tree_entropy, type=3)

# Computing prediction accuracy with testing data for Entropy Tree.

pred_values2 = predict(tree_entropy, test)
test2 = cbind(test, pred_values2)

tp2 = matrix(NA, nrow=nrow(test), ncol=99)
tn2 = matrix(NA, nrow=nrow(test), ncol=99)

for (i in 1:99) {
    tp2[,i] = ifelse(test$fraud=="1" & test$"1">0.01*i,1,0)
    tn2[,i] = ifelse(test$fraud=="0" & test$"1"<=0.01*i,1,0)
}

trueclassrate2 = matrix(NA, nrow=99, ncol=2)
for (i in 1:99) {
    trueclassrate2[i,1] = 0.01*i
    trueclassrate2[i,2] = sum(tp2[,i]+tn2[,i])/nrow(test)
}

print(trueclassrate2[which(trueclassrate2[,2]==max(trueclassrate2[,2])),])

card_data = mutate(card_data, distance_from_home_cat=ntile(distance_from_home, 10),
distance_from_last_transaction_cat=ntile(distance_from_last_transaction, 10),
ratio_to_median_purchase_price_cat=ntile(ratio_to_median_purchase_price, 10))

# Splitting data into 80% training and 20% testing sets.

set.seed(590520)
sample = sample(c(T,F), nrow(card_data), replace=T, prob=c(0.8, 0.2))
train = card_data[sample,]
test = card_data[!sample,]

# Fitting CHAID tree.

tree_CHAID = chaid(as.factor(fraud)~as.factor(distance_from_home_cat)
+as.factor(distance_from_last_transaction_cat)+as.factor(ratio_to_median_purchase_price_cat)
+as.factor(repeat_retailer)+as.factor(used_chip)+as.factor(used_pin_number)
+as.factor(online_order), data=train, control=chaid_control(maxheight=3))

plot(tree_CHAID, type="simple")

# Computing prediction accuracy for testing data for CHAID tree.

pred_values3 = predict(tree_CHAID, newdata=test)
test3 = cbind(test, pred_values3)

tp3 = matrix(NA, nrow=nrow(test), ncol=99)
tn3 = matrix(NA, nrow=nrow(test), ncol=99)

for (i in 1:99) {
    tp3[,i] = ifelse(test$fraud=="1" & test[[1]]>0.01*i,1,0)
    tn3[,i] = ifelse(test$fraud=="0" & test[[1]]<=0.01*i,1,0)
}

trueclassrate3 = matrix(NA, nrow=99, ncol=2)
for (i in 1:99) {
    trueclassrate3[i,1] = 0.01*i
    trueclassrate3[i,2] = sum(tp3[,i]+tn3[,i])/nrow(test)
}

print(trueclassrate3[which(trueclassrate3[,2]==max(trueclassrate3[,2])),])