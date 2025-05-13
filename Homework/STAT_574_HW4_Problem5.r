# STAT 574 HW4 Problem 5

# install.packages("tibbletime")
# install.packages("anomalize")
# install.packages("tidyverse")
library(readr)
library(tibbletime)
library(anomalize)
library(tidyverse)

wheat_data = read.csv("C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hw4STAT574S25/wheat_data.csv",
header=T, sep=",")

wheat_data$Date = as.Date(wheat_data$Date, "%m/%d/%Y")

wheat_data_tbl = as_tbl_time(wheat_data, Date)

print(wheat_data_tbl %>% time_decompose(Close, method="stl") %>% anomalize(remainder,
method="iqr") %>% time_recompose() %>% plot_anomalies(time_recomposed=T, color_no='blue',
color_yes='red', fill_ribbon='skyblue', size_circles=4) +
labs(title="Anomalies in Daily Closing Prices of Wheat", subtitle="1/4/2000-4/8/2022"))