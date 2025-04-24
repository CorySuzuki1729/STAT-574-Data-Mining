# STAT 574 Final Project Code (R)

library(readr)
library(dplyr)
library(keras3)

spam_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/PhiUSIIL_Phishing_URL_Dataset.csv",
header=T, sep=",")

spam_data = subset(spam_data, select=-c(FILENAME, URL, Domain, Title,
HasObfuscation, NoOfObfuscatedChar, ObfuscationRatio, TLD, 
NoOfAmpersandInURL, TLDLength))

scale01 <- function(x) {
    (x-min(x))/(max(x)-min(x))
}

spam_data = spam_data %>% mutate_all(scale01)

# Split the data into 80% training, 10% validation, and 10% testing sets. 

set.seed(312329)
sample = sample(c(T,F), nrow(spam_data), replace=T, prob=c(0.8,0.2))
train = spam_data[sample,]
split = spam_data[!sample,]

set.seed(251820)
sample2 = sample(c(T,F), nrow(split), replace=T, prob=c(0.5, 0.5))
validation = split[sample2,]
test = split[!sample2,]

train_x = data.matrix(train[-46])
train_y = data.matrix(train[46])
validation_x = data.matrix(validation[-46])
validation_y = data.matrix(validation[46])
test_x = data.matrix(test[-46])
test_y = data.matrix(test[46])

# Fitting AFNN model in R using the Keras library.

model_A = keras_model_sequential()
model_A %>% layer_dense(units=25, activation="relu", 
input_shape=ncol(train_x), 
kernel_regularizer=regularizer_l2(0.001),
kernel_initializer="he_normal")
model_A %>% layer_dropout(rate=0.5)
model_A %>% layer_dense(units=16, activation="relu",
kernel_regularizer=regularizer_l2(0.001),
kernel_initializer="he_normal")
model_A %>% layer_dropout(rate=0.5)
model_A %>% layer_dense(units=4, activation="relu",
kernel_regularizer=regularizer_l2(0.001),
kernel_initializer="he_normal")
model_A %>% layer_dropout(rate=0.5)
model_A %>% layer_dense(1, activation="sigmoid")

model_A %>% compile(loss="binary_crossentropy",
optimizer=optimizer_adam(learning_rate=0.0007),
metrics=c("accuracy"))

model_A %>% fit(train_x, train_y, batch_size=32, epochs=20)

pred_prob_A = model_A %>% predict(test_x)
match = c()
pred_prob_class = c()
for (i in 1:nrow(pred_prob_A)) {
    pred_prob_class[i] = ifelse(pred_prob_A[i]>0.5,1,0)
}
combined = cbind(test_y, pred_prob_class)
for (i in 1:nrow(combined)) {
    match[i] = ifelse(combined[i,1]==combined[i,2],1,0)
}
accuracy = sum(match)/nrow(combined)
print(round(accuracy, 4))