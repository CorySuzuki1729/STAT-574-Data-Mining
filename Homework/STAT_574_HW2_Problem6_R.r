library(xgboost)

concussion_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv",
header=T, sep=",")

# Splitting data into 80% training and 20% testing.

sample = sample(c(T,F), nrow(concussion_data), replace=T,
prob=c(0.8, 0.2))
train = concussion_data[sample,]
test = concussion_data[!sample,]

train_x = data.matrix(train[-4])
train_y = data.matrix(train[4])
test_x = data.matrix(test[-4])
test_y = data.matrix(test[4])

# Fitting gradient boosting multinomial classifier. 

xgb_multi = xgboost(data=train_x, label=train_y, max.depth=6,
eta=0.1, subsample=0.8, colsample_bytree=0.5,
nrounds=1000, num_class=4, objective="multi:softprob")

# Displaying feature importance. 

print(xgb.importance(colnames(train_x), model=xgb_multi))

# Computing prediction accuracy for testing data. 

pred_prob = predict(xgb_multi, test_x, reshape=T)
pred_prob = as.data.frame(pred_prob)
colnames(pred_prob) <- 0:3
pred_class = apply(pred_prob, 1, function(x)
colnames(pred_prob)[which.max(x)])

match = c()
n = length(test_y)
for (i in 1:n) {
    match[i] = ifelse(pred_class[i]==as.character(test_y[i]), 1, 0)
}

print(accuracy <- mean(match))