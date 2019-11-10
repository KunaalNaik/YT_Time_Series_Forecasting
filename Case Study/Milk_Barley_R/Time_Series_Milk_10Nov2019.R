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


#Subsetting the time series using window function
barTr<-window(bar,start=c(1884),end=c(1924),frequency=1)
plot(barTr)
barT<-window(bar,start=c(1925),end=c(1939),frequency=1)
plot(barT)

#Decomposing a timesries using ma and stl()
#Importing new data
ml<-read.csv("milk.csv",stringsAsFactors = F)
ml<-ml[-1]
ml<-ml[-158,1]
ml<-as.numeric(ml)
sum(is.na(ml))
ml<-ml[-157]
ml<-ts(ml,start=c(1962,1),frequency=12)
plot.ts(ml)

mlTr<-window(ml,start=c(1962,1),end=c(1972,1),frequency=12)
mlT<-window(ml,start=c(1972,2),end=c(1974,12),frequency=12)
#Data has both trend and seasonality

#Decomposing using MA
dec<-decompose(mlTr,type="additive")
dec$x #original series
dec$seasonal #seasonal indices (guessed based on frequency of ts() object)
dec$trend #trend component
dec$random #random component
dec$figure #seasonal components
dec$type # type of seasonality
plot(dec)

decM<-decompose(mlTr,type="multiplicative")
decM$x #original series
decM$seasonal #seasonal indices (guessed based on frequency of ts() object)
decM$trend #trend component
decM$random #random component
decM$figure #seasonal components
decM$type # type of seasonality
plot(decM)
#Decomposing using stl() and forecasting
#Additive model
dstl<-stl(mlTr,s.window = "periodic")
dstl$time.series#decomposed series

#Multiplicative model
decLM<-stl(log(mlTr),s.window = "periodic")
exp(decLM$time.series[,1])#seasonal components extracted
unique(exp(decLM$time.series[,1]))#Monthly indices

#exponential smoothing
es<-ses(mlTr,h=10)
accuracy(es,x =window(ml,start=c(1972,2),end=c(1972,11)))

#Fitting holts's model
hol<-holt(mlTr,h=10) #forecasting for 10 periods
accuracy(hol,x =window(ml,start=c(1972,2),end=c(1972,11)))

#Fitting holt's dampened trend model
holD<-holt(mlTr,h=10,damped = T)
accuracy(holD,x =window(ml,start=c(1972,2),end=c(1972,11)))

#Clearly the series is seasonal so seasonal component is required
hw<-hw(mlTr,h=10,seasonal = "additive")
accuracy(hw,x =window(ml,start=c(1972,2),end=c(1972,11)))

#Multiplicative season
hwM<-hw(mlTr,h=10,seasonal="multiplicative")
accuracy(hwM,x =window(ml,start=c(1972,2),end=c(1972,11)))

#Try damped trend
hwAD<-hw(mlTr,h=10,seasonal = "additive",damped=T)
accuracy(hwAD,x =window(ml,start=c(1972,2),end=c(1972,11)))

#Automating model building using ets()
auto<-ets(mlTr)
summary(auto)

foc<-forecast(auto,h=10)
accuracy(foc,x =window(ml,start=c(1972,2),end=c(1972,11)))
Box.test(auto$residuals)

##ARIMA Models###
library(tseries)
#Building ARIMA Models, non seasonal data
intusage<-WWWusage
plot(intusage)
adf.test(intusage)
plot(diff(intusage))
par(mfrow=c(2,1))
Acf(diff(intusage))
Pacf(diff(intusage)) #ARIMA(3,1,0)
dev.off()
modA<-Arima(y = intusage,order = c(3,1,0))
modA<-Arima(y = intusage,order = c(2,1,0))
modA<-Arima(y = intusage,order = c(2,1,1))
forecast(modA,h=10)
plot(forecast(modA,h=10))

#weather or not there is remaining structure in the series 
Box.test(modA$residuals)
