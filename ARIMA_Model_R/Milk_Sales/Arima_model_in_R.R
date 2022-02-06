#Import Libraries
library(forecast)
library(tseries)

#Change working Directory
setwd("D:\\GitHub\\YT_Time_Series_Forecasting\\ARIMA_Model_R")

#Import Sales  Dataset
sales <- read.csv("sales.csv")

#Convert sales_k column to Time Series object - 1971 to 1984
sales_ts <- ts(sales$Sales_k,start=c(1972),frequency=12)

#Plot Sales Time series using autoplot (forecast library)
autoplot(sales_ts)

#Stationarity : A stationary process has a mean and variance 
#               that do not change overtime and the process does not have trend.

#Perform ADF Test
#Null Hypothesis - Non Stationary 
#               (Do NOT Reject if P value > sig lvl (1%, 5%, 10%) )
adf.test(sales_ts, k= 12)

#Null Hypothesis not Rejected - Series is Non Stationary

#We will use first order difference to make it Stationary
sales_ts_d1 <- diff(sales_ts, differences = 1)
adf.test(sales_ts_d1, k=12)

#Still not Stationary; We will take 2nd order Difference
sales_ts_d1 <- diff(sales_ts, differences = 2)
adf.test(sales_ts_d1, k=12)

#Check plot
autoplot(sales_ts_d1)

#Since P is very small and less than sig lvl - 
#                 we accept alternate hypothesis

#ARIMA (p,d,q)


#Run PACF test to select AR term or the p term - correlation between lags
#Pacf(sales_ts) 
Pacf(sales_ts_d1) #choosing 7

#Run ACF test to select MA term or the q term - 
#Acf(sales_ts)
Acf(sales_ts_d1) #choosing 6

#BASIC ARIMA - does not work that good
tsMod <- Arima(y = sales_ts,order = c(7,2,6))

#Summary of the model
tsMod

#Forecast 12 periods ahead (1985)
forecast(tsMod,h=12)

#Plot Sales with forecast 
autoplot(forecast(tsMod,h=12))


