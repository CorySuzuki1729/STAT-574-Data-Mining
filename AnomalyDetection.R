#DOWNLOADING HISTORICAL DATA ON CRUDE OIL CLOSING PRICES

library(yahoofinancer)

crude.oil <- Ticker$new('CL=F')
crudeoil<- crude.oil$get_history(start = '2021-03-09', 
end = '2025-03-06', interval = '1d')

crudeoil$date<- substr(as.POSIXct(strftime(crudeoil$date, 
format="%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d", tz=""), 1, 10)

crudeoil.data<- as.data.frame(list(crudeoil$date, round(crudeoil$close,2)),
col.names=c("Date", "Close"), make.names=FALSE)

crudeoil.data<- na.omit(crudeoil.data)

crudeoil.data$Date<- as.Date(crudeoil.data$Date, format="%Y-%m-%d")

#RUNNING ANOMALY DETECTION ANALYSIS
library(tibbletime) #creates indices for date in time series data
crudeoil.data_tbl <- as_tbl_time(crudeoil.data, Date)

library(anomalize) 
library(tidyverse)
crudeoil.data_tbl %>% time_decompose(Close, method="stl") %>% 
anomalize(remainder, method="iqr", alpha=0.07) %>% time_recompose() %>% 
plot_anomalies(time_recomposed=TRUE, color_no='navy', 
color_yes='red',fill_ribbon='gray', size_circles=4) + 
labs(title="Anomalies in Daily Closing Prices of Crude Oil", 
subtitle="3/8/2021-3/6/2025") 
