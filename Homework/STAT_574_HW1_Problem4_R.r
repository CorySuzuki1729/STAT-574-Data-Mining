#install.packages("Hmisc")
library(readr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(partykit)
library(CHAID)
library(Hmisc)

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

# Computing confusion matrices and performance measures for testing set
# for a range of cutoffs.

pred_values = predict(tree_gini, test)
test = cbind(test, pred_values)

tpos = matrix(NA, nrow=nrow(test), ncol=102)
fpos = matrix(NA, nrow=nrow(test), ncol=102)
tneg = matrix(NA, nrow=nrow(test), ncol=102)
fneg = matrix(NA, nrow=nrow(test), ncol=102)

for (i in 0:101) {
    tpos[,i+1] = ifelse(test$fraud=="1" & test$"1">=0.01*i,1,0)
    fpos[,i+1] = ifelse(test$fraud=="0" & test$"1">=0.01*i,1,0)
    tneg[,i+1] = ifelse(test$fraud=="0" & test$"1"<0.01*i,1,0)
    fneg[,i+1] = ifelse(test$fraud=="1" & test$"1"<0.01*i,1,0)
}

tp = c()
fp = c()
tn = c()
fn = c()
accuracy = c()
misclassrate = c()
sensitivity = c()
specificity = c()
oneminusspec = c()
cutoff = c()

for (i in 1:102) {
    tp[i] = sum(tpos[,i])
    fp[i] = sum(fpos[,i])
    tn[i] = sum(tneg[,i])
    fn[i] = sum(fneg[,i])
    total = nrow(test)
    accuracy[i] = (tp[i]+tn[i])/total
    misclassrate[i] = (fp[i]+fn[i])/total 
    sensitivity[i] = tp[i]/(tp[i]+fn[i])
    specificity[i] = tn[i]/(fp[i]+tn[i])
    oneminusspec[i] = fp[i]/(fp[i]+tn[i])
    cutoff[i] = 0.01*(i-1)
}

# Plotting ROC Curve

plot(oneminusspec, sensitivity, type="l", lty=1, main="ROC Curve",
xlab="1-specificity", ylab="Sensitivity")
points(oneminusspec, sensitivity, pch=0)

# Reporting measures for the point on the ROC Curve closest to
# the ideal point (0,1).

distance = c()
for (i in 1:102) {
    distance[i] = sqrt(oneminusspec[i]^2+(1-sensitivity[i])^2)
}

measures = cbind(accuracy, misclassrate, sensitivity, specificity,
distance, cutoff)
min_dist = min(distance)
print(measures[which(measures[,5]==min_dist),])

# Computing the area under the ROC Curve

sensitivity = sensitivity[order(sensitivity)]
oneminusspec = oneminusspec[order(oneminusspec)]

lagx = Lag(oneminusspec, shift=1)
lagy = Lag(sensitivity, shift=1)
lagx[is.na(lagx)] = 0
lagy[is.na(lagy)] = 0
trapezoid = (oneminusspec-lagx)*(sensitivity+lagy)/2
print(AUC <- sum(trapezoid))