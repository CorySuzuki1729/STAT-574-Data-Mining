housing.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/housing_data.csv", 
header=TRUE, sep=",")

housing.data$ocean_proximity<- ifelse(housing.data$ocean_proximity=='<1H OCEAN',
1, ifelse(housing.data$ocean_proximity=='INLAND',2, 
ifelse(housing.data$ocean_proximity=='NEAR BAY',3,4)))

#SCALING VARIABLES TO FALL IN [0,1]
library(dplyr)

scale01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

housing.data<- housing.data %>% mutate_all(scale01)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
library(caTools)
set.seed(346634) 
sample<- sample.split(housing.data, SplitRatio=0.8)
train<- subset(housing.data, sample==TRUE)
test<-  subset(housing.data, sample==FALSE)

train.x<- data.matrix(train[-8])
train.y<- data.matrix(train[8])
test.x<- data.matrix(test[-8])
test.y<- data.matrix(test[8])

#FITTING ANN WITH LOGISTIC ACTIVATION FUNCTION
#install.packages("neuralnet")
library(neuralnet)
ann.log.reg<- neuralnet(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity,  
data=train, hidden=3, act.fct="logistic", stepmax=1e7) 

#PLOTTING THE DIAGRAM
plot(ann.log.reg)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.y<- predict(ann.log.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0)

print('Prediction Accuracy')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, test.y, type="l", lwd=2, col="magenta", main="ANN Regression with 
Logistic Activation Function", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
points(x,test.y, pch=16, col="magenta")
points(x, pred.y, pch=16, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("magenta","dodgerblue"))

#################################################################
#FITTING ANN WITH TANH ACTIVATION FUNCTION
ann.tanh.reg<- neuralnet(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity,  
data=train, hidden=2, act.fct="tanh", stepmax=1e7) 

#PLOTTING THE DIAGRAM
plot(ann.tanh.reg)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.y<- predict(ann.tanh.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0)

print('Prediction Accuracy')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, test.y, type="l", lwd=2, col="orange", main="ANN Regression with
Tanh Activation Function", panel.first=grid())
lines(x, pred.y, lwd=2, col="purple")
points(x,test.y, pch=16, col="orange")
points(x, pred.y, pch=16, col="purple")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("orange","purple"))

