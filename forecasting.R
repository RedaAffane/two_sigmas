#library(rhdf5)
#h5ls("./data/train.h5")
#sigma <- h5read("./data/train.h5", "train")
#head(sigma,1)

library(TTR)
library(forecast)
data = read.csv("./data/time_series_id0.csv")
train=data[data$timestamp<1200,]
test=data[data$timestamp>1199,]
time_series=ts(data$signal)
time_series
plot.ts(time_series,col="blue")
par(new=TRUE)
plot.ts(cumsum(time_series),col="red")
#Simple moving average
plot.ts(SMA(time_series,n=20))

#Seanonal data
time_series_components =decompose(time_series)

#Exponential smoothing: Simple exponential smoothing
train_series=ts(train$signal)
test_series=ts(test$signal)
train_holt = HoltWinters(train_series, beta=FALSE, gamma=FALSE,l.start = 0)
train_holt$SSE
plot(train_holt)
holt_forecasts=forecast.HoltWinters(train_holt,h=613)
holt_forecasts
plot.forecast(holt_forecasts)

rsquared=function(actual,predict){
  return(1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2)))
}

rsquared(test$signal,holt_forecasts$mean)
rsquared(test$signal,mean(test$signal))

acf(holt_forecasts$residuals[2:length(holt_forecasts$residuals)],lag.max = 20)
Box.test(holt_forecasts$residuals[2:length(holt_forecasts$residuals)], lag=20, type="Ljung-Box")
par(mfrow=c(1,2))
plot.ts(holt_forecasts$residuals)
plot.ts(train_series)
par(mfrow=c(1,1))
hist(holt_forecasts$residuals,breaks = 40)
shapiro.test(holt_forecasts$residuals)

#Exponential smoothing: Holt
holt = HoltWinters(train_series, gamma=FALSE,l.start = 0)
holt$SSE
holt_f=forecast.HoltWinters(train_holt,h=613)
holt_f
plot.forecast(holt_f)
rsquared(test$signal,holt_f$mean)

library(fUnitRoots)
unitrootTest(data$signal)

train_series_dif=diff(train_series, differences=2)
plot(train_series_dif)
plot(train_series)

library(tseries)
adf.test(train_series)
PP.test(train_series)
kpss.test(train_series)
#All test show that the series is stationnary

acf(train_series,lag.max = 30)
pacf(train_series,lag.max = 30)
auto.arima(train_series)

arima = arima(train_series,order=c(1,0,0))
arima_forecast = forecast.Arima(arima,h=613)
plot.forecast(arima_forecast)
rsquared(test$signal,arima_forecast$mean)

#Same analysis on sumulative series
#Exponential smoothing: Simple exponential smoothing
train_series_cum=cumsum(ts(train$signal))
test_series_cum=cumsum(ts(test$signal))

train_holt_cum = HoltWinters(train_series_cum, gamma=FALSE)
train_holt_cum$SSE
plot(train_holt_cum)
holt_forecasts=forecast.HoltWinters(train_holt_cum,h=613)
holt_forecasts
plot.forecast(holt_forecasts)

predicted=holt_forecasts$mean[2:length(holt_forecasts$mean)]-holt_forecasts$mean[1:length(holt_forecasts$mean)-1]
predicted=c(predicted[1],predicted)

rsquared(test$signal,predicted)
