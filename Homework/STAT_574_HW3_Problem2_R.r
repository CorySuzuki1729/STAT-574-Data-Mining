# STAT 574 HW3 Problem 2

library(readr)
library(e1071)

concussion_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv",
header=T, sep=",")

# Splitting the data into 80% training and 20% testing sets. 

set.seed(250217)
sample = sample(c(T,F), nrow(concussion_data), replace=T, prob=c(0.8, 0.2))
train = concussion_data[sample,]
test = concussion_data[!sample,]

# Fitting a multinomial Naive Bayes classifier. 

nb_multi = naiveBayes(as.factor(concussion)~age+nyearsplaying+position+prevconc,
data=train)

# Computing prediction accuracy for testing data. 

y_pred = predict(nb_multi, newdata=test)
len = nrow(test)
test = cbind(test, y_pred)
match = c()

for (i in 1:len) {
    match[i] = ifelse(test$concussion[i]==test$y_pred[i], 1, 0)
}

print(paste("Accuracy:", round(mean(match), 2)*100, '%'))