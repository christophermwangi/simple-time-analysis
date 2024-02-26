#Time series analysis
library(tseries)

#using inbuit data
data("AirPassengers")

start(AirPassengers)
end(AirPassengers)

summary(AirPassengers)
frequency(AirPassengers)

time(AirPassengers)
cycle(AirPassengers)

#plots 
plot(AirPassengers)
#fitting a line 
abline(reg=lm(AirPassengers~time(AirPassengers)))

#printing the cycles in the years
cycle(AirPassengers)

#Aggregating the cycles and display a year on your trend
plot(aggregate(AirPassengers,FUN = mean))

#boxplot across months will give us a sense on sasonal effects 
boxplot(AirPassengers~cycle(AirPassengers))

#arima model is commonly used in time series 
#since the data is not stationary --the mean is not constant and variance and covariance 
#one, we need to remove unequal variance--log 
#two,we need to adress the trend component

plot(diff(log(AirPassengers)))

#using dickey-fuller test to confir stationarity
adf.test(diff(log(AirPassengers)),alternative = c("stationary","explosive"),k=trunc(length(diff(log(AirPassengers))-1)^(1/3)))


#AR  I     MA
#P   DIFF  D
acf(diff(log(AirPassengers)))  #determines the value of q
pacf(diff(log(AirPassengers)))  #determines the value of p


#fit arima model and predict 10 years 
fit<-arima(log(AirPassengers),c(0,1,1),seasonal = list(order = c(0,1,1),period = 12))

pred<-predict(fit,n.ahead = 10*12)
#since the values are loged with raise them to exponential
predit<-2.718^pred$pred
ts.plot(AirPassengers,2.718^pred$pred,log="y",lty=c(1,3))

#testing our model
datawide<-ts(AirPassengers,frequency = 12,start = c(1949,1),end = c(1959,12))

fit2<-arima(log(datawide),c(0,1,1),seasonal = list(order = c(0,1,1),period = 12))

pred2<-predict(fit2,n.ahead = 10*12)
predit2<-2.718^pred2$pred
data1<-head(predit2,12)

predict_1960<-round(data1,digits = 0)
original_1960<-tail(AirPassengers,12)

