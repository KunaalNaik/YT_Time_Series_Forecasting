#Reading regular timeseries data
#Decomposing a time series using MA and stl()
#Forecasting using stl()
#Smoothning using holt, holtwinters and ets()
#Automating model selection using ets()
#Forecasting using holt, holtwinters and ets()
#ARIMA models, ACF , PACF, auto.arima()
#Choosing amongst different models by using accuracy() function
library(forecast)
library(tseries)

###Reading time series data inside R###
setwd("C:\\Users\\DELL\\Documents\\Jigsaw Courses\\Courses\\Predictive Analytics with R\\Time Series in R - Class codes")
bar<-read.csv("barley.csv")
bar<-bar[-1]
bar<-ts(bar,start=c(1884),frequency=1)
plot(bar)