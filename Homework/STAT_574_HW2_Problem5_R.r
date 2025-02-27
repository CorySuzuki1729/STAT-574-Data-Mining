library(xgboost)

card_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv",
header=T, sep=",")

# Splitting data into 80% training and 20% testing sets.

set.seed(364663)
sample = sample(c(T,F), nrow(card_data), replace=T, prob=c(0.8, 0.2))
train = card_data[sample,]
test = card_data[!sample,]

train_x = data.matrix(train[-7])
train_y = data.matrix(train[7])
test_x = data.matrix(test[-7])
test_y = data.matrix(test[7])

# Fitting gradient boosted binary classifier. 

xgb_bin = xgboost(data=train_x, label=train_y,
max.depth=8, eta=0.1, subsample=0.8, colsample_bytree=0.5,
nrounds=1000, objective="binary:logistic")

# Displaying feature importance. 

print(xgb.importance(colnames(train_x), model=xgb_bin))

# Computing prediction accuracy for testing data. 

pred_prob = predict(xgb_bin, test_x)

len = length(pred_prob)
pred_card = c()
match = c()
for (i in 1:len){
    pred_card[i] = ifelse(pred_prob[i]>=0.5, 1, 0)
    match[i] = ifelse(test_y[i]==pred_card[i], 1, 0)
}

print(prop <- sum(match)/len)