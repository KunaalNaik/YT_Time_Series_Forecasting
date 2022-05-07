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
setwd("D:\\OneDrive\\_YouTube Working Files\\Git Time Series\\YT_Time_Series_Forecasting\\ARIMA_Model_R\\Barley")
bar<-read.csv("barley.csv")
# Select Only the Time Series Data
bar<-bar[-1]
# Configure the Period
bar<-ts(bar,start=c(1884),frequency=1)
# Plot ts Data
plot(bar)


### Subset Data
# train
bar_train <- window(bar, start = c(1884), end=c(1924), frequency = 1)
plot(bar_train)
# test
bar_test <- window(bar, start = c(1925), end=c(1939), frequency = 1)
plot(bar_test)

### Import Milk Data - which is more granular - monthly
milk<-read.csv("milk.csv")
# Select Only the Time Series Data
milk<-milk[-1]
# Configure the Period
milk<-ts(milk,start=c(1962,1),frequency=12)
# Plot ts Data
plot(milk)

# train
milk_train <- window(milk, start = c(1962,1), end=c(1972,1), frequency = 12)
plot(milk_train)
# test
milk_test <- window(milk, start = c(1972,2), end=c(1974,12), frequency = 12)
plot(milk_test)

### Decompose - using MA
milk_train_dec <- decompose(milk_train, type = "additive")

plot(milk_train_dec$seasonal)
plot(milk_train_dec$trend)
plot(milk_train_dec$random)
plot(milk_train_dec)

autoplot(milk_train_dec)

### Build - Check Accuracy

## Simple Exponential Model
es_milk <- ses(milk_train, h = 10)
plot(es_milk)
accuracy(es_milk, x = milk_test)

## Plain Holt's Winter
hw_milk <- holt(milk_train, h = 10)
plot(hw_milk)
accuracy(hw_milk, x = milk_test)

## Plain Damped Holt's Winter
hw_milk <- holt(milk_train, h = 10, damped = T)
plot(hw_milk)
accuracy(hw_milk, x = milk_test)

## Holt's Winter - Additive
hw_milk <- hw(milk_train, h = 10, seasonal = "additive")
plot(hw_milk)
accuracy(hw_milk, x = milk_test)

## Holt's Winter - multiplicative
hw_milk <- hw(milk_train, h = 10, seasonal = "multiplicative")
plot(hw_milk)
accuracy(hw_milk, x = milk_test)

## Holt's Winter - Additive and Damped
hw_milk <- hw(milk_train, h = 10, seasonal = "additive", damped = T)
plot(hw_milk)
accuracy(hw_milk, x = milk_test)


## Automating Model Building using ets()
auto <- ets(milk_train)
summary(auto)

# Forecast
auto_forecast <- forecast(auto, h = 10)
accuracy(auto_forecast, x = milk_test)
#Box.test(auto$residuals)

###ARIMA

## Get Data
usage <- WWWusage
plot(usage)

# ADF test - Stationarity test 
adf.test(usage)

# Plot Diff
plot(diff(usage))

adf.test(diff(usage, differences = 2))

# Choice of Diff = 2

# AR Term
pacf(diff(usage, differences = 2))

# Select AR = 2 based on chart

# MA Term
acf(diff(usage, differences = 2))

# Select MA = 2 based on chart

m_Arima <- arima(x = usage, order = c(2,2,2))
m_Arima


# Forecast
forecast(m_Arima, h = 10)
plot(forecast(m_Arima, h = 10))







