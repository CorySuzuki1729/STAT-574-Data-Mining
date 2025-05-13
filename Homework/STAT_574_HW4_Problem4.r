# STAT 574 HW4 Problem 4

install.packages("changepoint")
library(readr)
library(changepoint)

gold_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/wheat_data.csv",
header=T, sep=",")

# Detection of change points for change in mean. 

mean_det = cpt.mean(gold_data$Close, penalty="AIC", method="BinSeg", Q=3)
plot(mean_det, cpt.col="red", ylab="Daily Closing Price", main="Change Point Detection for Change in Mean")
paste("Change Point Locations: ", paste(mean_det@cpts, collapse=", "))

# Detection of change points for change in variance. 

var_det = cpt.var(gold_data$Close, penalty="AIC", method="BinSeg", Q=3)
plot(var_det, cpt.col="red", ylab="Daily Closing Price", main="Change Point Detection for Change in Variance")
paste("Change Point Locations: ", paste(var_det@cpts, collapse=", "))

# Detection of Change Points for change in mean and variance. 

mean_var_det = cpt.meanvar(gold_data$Close, penalty="AIC", method="BinSeg", Q=3)
plot(mean_var_det, cpt.col="red", ylab="Daily Closing Price",
main="Change Point Detection for Change in Mean and Variance")
paste("Change Point Locations: ", paste(mean_var_det@cpts, collapse=", "))