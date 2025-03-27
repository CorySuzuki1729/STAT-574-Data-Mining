pneumonia.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv", 
header=TRUE, sep=",")

#pneumonia.cat<- ifelse(pneumonia.data$pneumonia=="yes",1,0)
#pneumonia.data$pneumonia<- relevel(as.factor(pneumonia.cat), ref="0")
pneumonia.data$pneumonia<- relevel(as.factor(pneumonia.data$pneumonia), ref="no")
pneumonia.data$gender<- ifelse(pneumonia.data$gender=='M',1,0)
pneumonia.data$tobacco_use<- ifelse(pneumonia.data$tobacco_use=='yes',1,0) 


#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
library(caTools)
set.seed(503548)
sample<- sample.split(pneumonia.data, SplitRatio=0.8)
train<- subset(pneumonia.data, sample==TRUE)
test<-  subset(pneumonia.data, sample==FALSE)

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

library(neuralnet)

#FITTING ANN WITH LOGISTIC ACTIVATION FUNCTION AND ONE LAYER WITH THREE NEURONS
ann.log.class<- neuralnet(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, hidden=3, act.fct="logistic", stepmax=1e7)

#PLOTTING THE DIAGRAM
plot(ann.log.class)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.prob<- predict(ann.log.class, test.x)[,1]

pred.y<- c()
match<- c()
for (i in 1:length(test.y)){
  pred.y[i]<- ifelse(pred.prob[i]>0.5,1,0)
  match[i]<- ifelse(test.y[i]==pred.y[i],1,0)
}

print(paste("accuracy=", round(mean(match), digits=4)))

####################################################################
#FITTING ANN WITH LOGISTIC ACTIVATION FUNCTION AND C(2,3) LAYERS
ann.log23.class<- neuralnet(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, hidden=c(2,3), act.fct="logistic", stepmax=1e7)

#PLOTTING THE DIAGRAM
plot(ann.log23.class)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.prob<- predict(ann.log23.class, test.x)[,1]

match<- c()
pred.y<- c()
for (i in 1:length(test.y)){
  pred.y[i]<- ifelse(pred.prob[i]>0.5,1,0)
  match[i]<- ifelse(test.y[i]==pred.y[i],1,0)
}

print(paste("accuracy=", round(mean(match), digits=4)))

####################################################################
#FITTING ANN WITH TANH ACTIVATION FUNCTION
ann.tanh.class<- neuralnet(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, hidden=3, act.fct="tanh", stepmax=1e7)

#PLOTTING THE DIAGRAM
plot(ann.tanh.class)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.prob<- predict(ann.tanh.class, test.x)[,1]

match<- c()
pred.y<- c()
for (i in 1:length(test.y)){
  pred.y[i]<- ifelse(pred.prob[i]>0.5,1,0)
  match[i]<- ifelse(test.y[i]==pred.y[i],1,0)
}

print(paste("accuracy=", round(mean(match), digits=4)))

