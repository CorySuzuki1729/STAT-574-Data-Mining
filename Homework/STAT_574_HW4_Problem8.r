# STAT 574 HW4 Problem 8

# install.packages("BiocManager")
# BiocManager::install("EBImage")
library(keras3)
library(EBImage)

# Preparing Data 

# Caracals

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/CARACALS")
img_caracals = sample(dir());
train_caracals = list(NULL);
for (i in 1:length(img_caracals)) {
    train_caracals[[i]] = readImage(img_caracals[i])
    train_caracals[[i]] = resize(train_caracals[[i]], 100, 100)
}

# Cheetahs

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/CHEETAHS")
img_cheetahs = sample(dir());
train_cheetahs = list(NULL);
for (i in 1:length(img_cheetahs)) {
    train_cheetahs[[i]] = readImage(img_cheetahs[i])
    train_cheetahs[[i]] = resize(train_cheetahs[[i]], 100, 100)
}

# Lions

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/LIONS")
img_lions = sample(dir());
train_lions = list(NULL);
for (i in 1:length(img_lions)) {
    train_lions[[i]] = readImage(img_lions[i])
    train_lions[[i]] = resize(train_lions[[i]], 100, 100)
}

# Tigers 

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/TIGERS")
img_tigers = sample(dir());
train_tigers = list(NULL);
for (i in 1:length(img_tigers)) {
    train_tigers[[i]] = readImage(img_tigers[i])
    train_tigers[[i]] = resize(train_tigers[[i]], 100, 100)
}

train_pool = c(train_caracals[1:40], train_cheetahs[1:40], train_lions[1:40], train_tigers[1:40])

# Permutation of image dimensions. 

train = aperm(combine(train_pool), c(4,1,2,3))

# Creating image labels. 

train_y = c(rep(0,40), rep(1,40), rep(2,40), rep(3,40))
train_lab = to_categorical(train_y)

# Preparing testing data. 

# Caracals

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/CARACALS")
img_caracals = sample(dir())
test_caracals = list(NULL)

for (i in 1:length(img_caracals)) {
    test_caracals[[i]] = readImage(img_caracals[i])
    test_caracals[[i]] = resize(test_caracals[[i]], 100, 100)
}

# Cheetahs

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/CHEETAHS")
img_cheetahs = sample(dir())
test_cheetahs = list(NULL)

for (i in 1:length(img_cheetahs)) {
    test_cheetahs[[i]] = readImage(img_cheetahs[i])
    test_cheetahs[[i]] = resize(test_cheetahs[[i]], 100, 100)
}

# Lions 

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/LIONS")
img_lions = sample(dir())
test_lions = list(NULL)

for (i in 1:length(img_lions)) {
    test_lions[[i]] = readImage(img_lions[i])
    test_lions[[i]] = resize(test_lions[[i]], 100, 100)
}

# Tigers

setwd("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/WildAnimalsImages/train/TIGERS")
img_tigers = sample(dir())
test_tigers = list(NULL)

for (i in 1:length(img_tigers)) {
    test_tigers[[i]] = readImage(img_tigers[i])
    test_tigers[[i]] = resize(test_tigers[[i]], 100, 100)
}

test_pool = c(test_caracals[1:3], test_cheetahs[1:3], test_lions[1:3], test_tigers[1:3])
test = aperm(combine(test_pool), c(4,1,2,3))
test_y = c(rep(0,3), rep(1,3), rep(2,3), rep(3,3))
test_lab = to_categorical(test_y)

# Fitting CNN Architecture Model. 

model_cnn = keras_model_sequential()

model_cnn %>% layer_conv_2d(filters=40, kernel_size=c(3,3),
activation="relu", input_shape=c(100,100,3)) %>%
layer_conv_2d(filters=40, kernel_size=c(3,3), activation="relu") %>%
layer_max_pooling_2d(pool_size=c(3,3)) %>% layer_dropout(rate=0.25) %>%
layer_conv_2d(filters=80, kernel_size=c(3,3), activation="relu") %>%
layer_conv_2d(filters=80, kernel_size=c(3,3), activation="relu") %>%
layer_max_pooling_2d(pool_size=c(3,3)) %>% layer_dropout(rate=0.35) %>%
layer_flatten() %>% layer_dense(units=256, activation="relu") %>%
layer_dropout(rate=0.25) %>% layer_dense(units=4, activation="softmax") %>%
compile(loss="categorical_crossentropy", optimizer=optimizer_adam(), metrics=c("accuracy"))

history = model_cnn %>% fit(train, train_lab, epochs=50, batch_size=40, validation_split=0.2)

# Computing prediction accuracy for testing set. 

model_cnn %>% evaluate(test, test_lab)

pred_class = as.array(model_cnn %>% predict(test) %>% k_argmax())
print(pred_class)
print(test_y)

print(paste("Accuracy: ", round(1-mean(test_y!=pred_class), digits=4)))