# STAT 574 HW4 Problem 2

library(readr)
library(dplyr)
library(keras3)

cvx_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/CVX_historical_data_shock.csv",
header=T, sep=",")

# Splitting data into training and testing sets. 

cvx_data$Year = as.numeric(format(as.Date(cvx_data$Date, format="%m/%d/%Y"), "%Y"))
train_data = cvx_data[which(cvx_data$Year<2022), 1:2]
test_data = cvx_data[which(cvx_data$Year>=2022), 1:2]

nsteps = 60
train_matrix = matrix(nrow=nrow(train_data)-nsteps, ncol=nsteps+1)
for (i in 1:(nrow(train_data)-nsteps)) {
    train_matrix[i,] = cvx_data$Shock[i:(i+nsteps)]
}
train_x = array(train_matrix[,-ncol(train_matrix)], dim=c(nrow(train_matrix), nsteps, 1))
train_y = train_matrix[,ncol(train_matrix)]

# Creating test_x and test_y

test_matrix = matrix(nrow=nrow(test_data), ncol=nsteps+1)
for (i in 1:nrow(test_data)) {
    test_matrix[i,] = cvx_data$Shock[(i+nrow(train_matrix)):(i+nsteps+nrow(train_matrix))]
}
test_x = array(test_matrix[,-ncol(test_matrix)], dim=c(nrow(test_matrix), nsteps, 1))
test_y = test_matrix[,ncol(test_matrix)]

# Fitting LSTM Model

LSTM_biclass = keras_model_sequential()
LSTM_biclass %>% layer_dense(input_shape=dim(train_x)[2:3], units=nsteps)
LSTM_biclass %>% layer_lstm(units=25)
LSTM_biclass %>% layer_dense(units=1, activation="sigmoid")
LSTM_biclass %>% compile(loss="binary_crossentropy")

LSTM_biclass %>% fit(train_x, train_y, batch_size=32, epochs=5)

# Computing prediction accuracy for testing data. 

pred_prob = LSTM_biclass %>% predict(test_x)
match = cbind(test_y, pred_prob)
tp = matrix(NA, nrow=nrow(match), ncol=99)
tn = matrix(NA, nrow=nrow(match), ncol=99)

for (i in 1:99) {
    tp[,i] = ifelse(match[,1]==1 & match[,2]>0.01*i, 1, 0)
    tn[,i] = ifelse(match[,1]==0 & match[,2]<=0.01*i, 1, 0)
}

trueclassrate = matrix(NA, nrow=99, ncol=2)
for (i in 1:99) {
    trueclassrate[i, 1] = 0.01*i
    trueclassrate[i, 2] = sum(tp[,i]+tn[,i])/nrow(match)
}

print(trueclassrate[which(trueclassrate[,2]==max(trueclassrate[,2])),])

# Fitting GRU model

gru_biclass = keras_model_sequential()
gru_biclass %>% layer_dense(input_shape=dim(train_x)[2:3], units=nsteps)
gru_biclass %>% layer_gru(units=25)
gru_biclass %>% layer_dense(units=1, activation="sigmoid")
gru_biclass %>% compile(loss="binary_crossentropy")

gru_biclass %>% fit(train_x, train_y, batch_size=32, epochs=5)

# Computing prediction accuracy for testing data. 

pred_prob_gru = gru_biclass %>% predict(test_x)
match_gru = cbind(test_y, pred_prob_gru)
tp_gru = matrix(NA, nrow=nrow(match_gru), ncol=99)
tn_gru = matrix(NA, nrow=nrow(match_gru), ncol=99)

for (i in 1:99) {
    tp[,i] = ifelse(match_gru[,1]==1 & match_gru[,2]>0.01*i, 1, 0)
    tn[,i] = ifelse(match_gru[,1]==0 & match_gru[,2]<=0.01*i, 1, 0)
}

trueclassrate_gru = matrix(NA, nrow=99, ncol=2)
for (i in 1:99) {
    trueclassrate_gru[i, 1] = 0.01*i
    trueclassrate_gru[i, 2] = sum(tp[,i]+tn[,i])/nrow(match_gru)
}

print(trueclassrate_gru[which(trueclassrate[,2]==max(trueclassrate[,2])),])