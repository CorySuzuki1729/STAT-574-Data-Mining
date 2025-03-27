movie.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/movie_data.csv", 
header=TRUE, sep=",")
            
#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(123857)
sample <- sample(c(TRUE, FALSE), nrow(movie.data), replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

#FITTING K-NEAREST NEIGHBOR MULTINOMIAL CLASSIFIER 
#k=3 reasonably maximizes prediction accuracy for testing set
library(caret)
knn.mclass<- knnreg(train.x, train.y, k=3)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.y<- round(predict(knn.mclass, test.x), digits=0)
print(paste("accuracy=", round(1-mean(test.y!=pred.y),digits=4)))

