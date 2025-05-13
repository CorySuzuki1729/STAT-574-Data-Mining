# STAT 574 HW4 Problem 1

# install.packages("keras3")
library(readr)
library(keras3)

cvx_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/CVX_historical_data.csv",
header=T, sep=",")

# Splitting data into training and testing sets. 

cvx_data$Year = as.numeric(format(as.Date(cvx_data$Date, format="%Y-%m-%d"), "%Y"))

train_data = cvx_data[which(cvx_data$Year<2022), 1:2]
test_data = cvx_data[which(cvx_data$Year>=2022), 1:2]

# Plotting training and testing data

plot(as.POSIXct(cvx_data$Date), cvx_data$Close, main="Daily Chevron Stock CLosing Prices",
xlab="Time", ylab="Stock Price", pch="", panel.first=grid())
lines(as.POSIXct(train_data$Date), train_data$Close, lwd=2, col="blue")
lines(as.POSIXct(test_data$Date), test_data$Close, lwd=2, col="green")
legend("topleft", c("training", "testing"), lty=1, lwd=2, col=c("blue", "green"))

# Scaling prices to fall within [0,1]

price = as.numeric(cvx_data$Close)
price_sc = (price-min(price))/(max(price)-min(price))

# Creating train_x and train_y

nsteps = 60
train_matrix = matrix(nrow=nrow(train_data)-nsteps, ncol=nsteps+1)
for (i in 1:(nrow(train_data)-nsteps)) {
    train_matrix[i,] = price_sc[i:(i+nsteps)]
}
train_x = array(train_matrix[,-ncol(train_matrix)], dim=c(nrow(train_matrix), nsteps, 1))
train_y = train_matrix[,ncol(train_matrix)]

# Creating test_x and test_y 

test_matrix = matrix(nrow=nrow(test_data), ncol=nsteps+1)
for (i in 1:nrow(test_data)) {
    test_matrix[i,] = price_sc[(i+nrow(train_matrix)):(i+nsteps+nrow(train_matrix))]
}
test_x = array(test_matrix[,-ncol(test_matrix)], dim=c(nrow(test_matrix), nsteps,1))
test_y = test_matrix[,ncol(test_matrix)]

# Fitting LSTM Model

LSTM_model = keras_model_sequential()

LSTM_model %>% layer_lstm(input_shape=dim(train_x)[2:3], units=nsteps)
LSTM_model %>% layer_dense(units=1, activation="tanh")
LSTM_model %>% compile(loss="mean_squared_error")

LSTM_model %>% fit(train_x, train_y, batch_size=32, epochs=5)
pred_y = LSTM_model %>% predict(test_x, batch_size=32)

test_y_re = test_y*(max(price)-min(price))+min(price)
pred_y_re = pred_y*(max(price)-min(price))+min(price)

# Computing prediction accuracy

accuracy10 = ifelse(abs(test_y_re-pred_y_re)<0.10*test_y_re, 1, 0)
accuracy15 = ifelse(abs(test_y_re-pred_y_re)<0.15*test_y_re, 1, 0)
accuracy20 = ifelse(abs(test_y_re-pred_y_re)<0.20*test_y_re, 1, 0)

print(paste("Accuracy within 10%:", round(mean(accuracy10), 4)))
print(paste("Accuracy within 15%:", round(mean(accuracy15), 4)))
print(paste("Accuracy within 20%:", round(mean(accuracy20), 4)))

# Plotting actual and predicted values for testing data

plot(as.POSIXct(test_data$Date), test_y_re, type="l", lwd=2, col="green", 
main="Daily Chevron Stock Actual and Predicted Prices - LSTM Model", 
xlab="Time", ylab="Stock Price", panel.first=grid())
lines(as.POSIXct(test_data$Date), pred_y_re, lwd=2, col="orange")
legend("bottomright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green","orange"))

# Fitting GRU Model

gru_model = keras_model_sequential()

gru_model %>% layer_gru(input_shape=dim(train_x)[2:3], units=nsteps)
gru_model %>% layer_dense(units=1, activation="tanh")
gru_model %>% compile(loss="mean_squared_error")

gru_model %>% fit(train_x, train_y, batch_size=32, epochs=5)

pred_y_gru = gru_model %>% predict(test_x, batch_size=32)

pred_y_re_gru = pred_y_gru*(max(price)-min(price))+min(price)

# Computing prediction accuracy

accuracy10_gru = ifelse(abs(test_y_re-pred_y_re_gru)<0.10*test_y_re, 1, 0)
accuracy15_gru = ifelse(abs(test_y_re-pred_y_re_gru)<0.15*test_y_re, 1, 0)
accuracy20_gru = ifelse(abs(test_y_re-pred_y_re_gru)<0.20*test_y_re, 1, 0)

print(paste("Accuracy within 10%:", round(mean(accuracy10_gru), 4)))
print(paste("Accuracy within 15%:", round(mean(accuracy15_gru), 4)))
print(paste("Accuracy within 20%:", round(mean(accuracy20_gru), 4)))

# Plotting actual and predicted values for testing data

plot(as.POSIXct(test_data$Date), test_y_re, type="l", lwd=2, col="green", 
main="Daily Chevron Stock Actual and Predicted Prices - GRU Model", 
xlab="Time", ylab="Stock Price", panel.first=grid())
lines(as.POSIXct(test_data$Date), pred_y_re_gru, lwd=2, col="orange")
legend("bottomright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green","orange"))