library(tseries)
library(forecast)
library(caret)

#Load Dataset directly
data(AirPassengers)

#Check class, it should be Time Series (ts)
class(AirPassengers)

#ts(values,start=c(1949,1),end=c(1960,3),frequency=12)

#Check First row
start(AirPassengers)

#Check Last Row
end(AirPassengers)

#Check Frequency (daily, weekly, monthly,quarterly, yearly)
frequency(AirPassengers)

#Check summary stats 
summary(AirPassengers)

#Check length
length(AirPassengers)

#Plot it on chart to inspect 
plot(AirPassengers)
plot(diff(log(AirPassengers)))

#Create a Linear model based on time to see the trend
abline(reg = lm(AirPassengers~time(AirPassengers)))

#Check Cycle
cycle(AirPassengers)

#Plot Average to see it trend (moving upwards)
plot(aggregate(AirPassengers,FUN=mean))

#Check plots according to cycle
boxplot(AirPassengers~cycle(AirPassengers))


#Build Arima

#Augmented Dickey-Fuller Test
adf.test(AirPassengers, alternative="stationary", k=0)
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

#It is stationary to do run ARIMA

#Find ACF
acf(AirPassengers)
acf(log(AirPassengers))

acf(diff(log(AirPassengers))) #p term or Lagged 
pacf(diff(log(AirPassengers))) #q term or Moving average

#Diff if need(that is when the chart slowly drops off)
#air_diff = diff(AirPassengers, differences = 1)
#plot(air_diff)

#Choose (p,d,q) (0,1,1) or (1,1,1)

(fit_test <- auto.arima(log(AirPassengers), seasonal = FALSE))
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

#ARIMAX, use xreg = 
#(fit <- arima(log(AirPassengers),xreg = <Columns>, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

pred_train <- predict(fit, )
pred <- predict(fit, n.ahead = 10*12)


ts.plot(AirPassengers,2.718^pred$pred, lty = c(1,3))
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

RMSE(AirPassengers, pred_train$pred)
