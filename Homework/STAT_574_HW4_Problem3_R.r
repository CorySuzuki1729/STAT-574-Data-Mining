# STAT 574 HW4 Problem 3

library(readr)
library(keras3)

weather_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/weather_description.csv")
table(DT <- weather_data$Seattle)

DT = ifelse(DT=="sky is clear", "clear", ifelse(DT %in% c("broken clouds", "few clouds", "overcast clouds", "scattered clouds", "smoke"), "cloudy",
ifelse(DT %in% c("heavy shower snow", "heavy snow", "light shower snow", "light snow", "shower snow", "snow"), "snow",
ifelse(DT %in% c("fog", "haze", "mist"), "fog", "rain"))))

weather_data$clear = ifelse(DT=="clear", 1, 0)
weather_data$cloudy = ifelse(DT=="cloudy",1,0)
weather_data$rain = ifelse(DT=="rain",1,0)
weather_data$snow = ifelse(DT=="snow",1,0)
weather_data$fog = ifelse(DT=="fog",1,0)
weather_data$year = format(as.Date(weather_data$datetime, format="%m/%d/%y"), "%Y")

rnn_model = function(modelname, varname) {
    train_data = weather_data[which(weather_data$year<2017), varname]
    test_data = weather_data[which(weather_data$year==2017), varname]
    nsteps = 60
    train_matrix = matrix(nrow=length(train_data)-nsteps, ncol=nsteps+1)
    for (i in 1:(length(train_data)-nsteps)) {
        train_matrix[i,] = weather_data[i:(i+nsteps), varname]
    }
    train_x = array(train_matrix[,-ncol(train_matrix)], dim=c(nrow(train_matrix), nsteps, 1))
    train_y = train_matrix[,ncol(train_matrix)]
    test_matrix = matrix(nrow=length(test_data), ncol=nsteps+1)
    for (i in 1:length(test_data)) {
        test_matrix[i,] = weather_data[(i+nrow(train_matrix)):(i+nsteps+nrow(train_matrix)), varname]
    }
    test_x = array(test_matrix[,-ncol(test_matrix)], dim=c(nrow(test_matrix), nsteps, 1))
    test_y = test_matrix[,ncol(test_matrix)]

    fitted_model = keras_model_sequential()
    fitted_model %>% layer_dense(input_shape=dim(train_x)[2:3], units=nsteps)
    if (modelname=="lstm") {
        fitted_model %>% layer_lstm(units=6)
    } else {
        fitted_model %>% layer_gru(units=6)
    }
    fitted_model %>% layer_dense(units=1, activation="sigmoid")
    fitted_model %>% compile(loss='binary_crossentropy')

    fitted_model %>% fit(train_x, train_y, batch_size=32, epochs=1)
    pred_prob = fitted_model %>% predict(test_x)
    return(list(test_y, pred_prob))
}

accuracy = function() {
    test_y = bind_cols(test_clear, test_cloudy, test_snow, test_fog, test_rain)
    colnames(test_y) = 1:5
    true_class = as.numeric(apply(test_y, 1, function(x)
    colnames(test_y)[which.max(x)]))
    pred_prob = bind_cols(pred_prob_clear, pred_prob_cloudy, pred_prob_snow, pred_prob_fog, pred_prob_rain)
    colnames(pred_prob) = 1:5
    pred_class = as.numeric(apply(pred_prob, 1, function(x)
    colnames(pred_prob)[which.max(x)]))
    match = c()
    for (i in 1:length(pred_class)) {
        match[i] = ifelse(pred_class[i] == true_class[i],1,0)
    }
    return(round(mean(match), 4))
}

# Running LSTM Binary Classification Models

list_clear = rnn_model('lstm', 'clear')
test_clear = list_clear[1]
pred_prob_clear = list_clear[2]

list_cloudy = rnn_model('lstm', 'cloudy')
test_cloudy = list_cloudy[1]
pred_prob_cloudy = list_cloudy[2]

list_snow = rnn_model('lstm', 'snow')
test_snow = list_snow[1]
pred_prob_snow = list_snow[2]

list_fog = rnn_model('lstm', 'fog')
test_fog = list_fog[1]
pred_prob_fog = list_fog[2]

list_rain = rnn_model('lstm', 'rain')
test_rain = list_rain[1]
pred_prob_rain = list_rain[2]

# Running GRU Binary Classification Models

list_clear = rnn_model('gru', 'clear')
test_clear = list_clear[1]
pred_prob_clear = list_clear[2]

list_cloudy = rnn_model('gru', 'cloudy')
test_cloudy = list_cloudy[1]
pred_prob_cloudy = list_cloudy[2]

list_snow = rnn_model('gru', 'snow')
test_snow = list_snow[1]
pred_prob_snow = list_snow[2]

list_fog = rnn_model('gru', 'fog')
test_fog = list_fog[1]
pred_prob_fog = list_fog[2]

list_rain = rnn_model('gru', 'rain')
test_rain = list_rain[1]
pred_prob_rain = list_rain[2]