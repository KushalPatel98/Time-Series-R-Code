## load packages
library('tidyverse')
library('forecast')
library('tseries')
library('xts')
library('astsa')

## load in data and create time series object
data <- read_csv('GPDIC1.csv', col_types = 'Dn')
data <- data[1:289,]
colnames(data) <- c('Date', 'i')
investment <- ts(data[,2],freq = 4)

## plot data, test for unit root and seasonality
plot.ts(investment, ylab = 'Investment (Billions of 2012 Chained Dollars)',main = 'Real Gross Private Investment 1960 - 2020')
adf.test(investment)
pp.test(investment)

log_inv <- log(investment)
diff_inv <- diff(log_inv)
log_inv_train <- ts(log_inv[1:251],freq = 4)
log_inv_test <- ts(log_inv[252:289],freq = 4)
plot(diff_inv)
adf.test(diff_inv)
pp.test(diff_inv)

ggmonthplot(log_inv)

## Plot ACF and PACF of the differenced log investment (indicates ARIMA(4,1,0))
acf2(diff(log_inv_train), max.lag = 48)

# Function for making ARIMA model
ts.model = function(ts, col = 'remainder', order = c(0,0,1),seasonal)
{
  mod = Arima(ts, order = order,seasonal = seasonal, include.mean = FALSE,include.drift = TRUE)
  mod
}

ts.model(log_inv_train,order = c(4,1,4), seasonal = list(order = c(1,0,1),4))
ts.model(log_inv_train,order = c(4,1,4), seasonal = list(order = c(2,0,2),4))
ts.model(log_inv_train,order = c(4,1,3), seasonal = list(order = c(1,0,1),4))
ts.model(log_inv_train,order = c(3,1,3), seasonal = list(order = c(1,0,1),4))



# Model Diagnostics
fit <- Arima(log_inv_train,order = c(4,1,4), seasonal = list(order = c(2,0,2),4),include.drift = TRUE)
fit 

tsdiag(fit)


qqnorm(residuals(fit))
qqline(residuals(fit))

# Forecasting

fcast=forecast(fit,h=38)
plot(fcast,ylab = 'Log Investment', xlab = 'Time')


holt_fit <- HoltWinters(log_inv_train)
holtcast <- forecast(holt_fit, h = 38)
plot(holtcast)



# Forecast Evaluation

HWerr=log_inv_test[1:length(log_inv_test)]-holtcast$mean
HWmae=mean(abs(HWerr))
HWrmse=sqrt(mean(HWerr^2))
HWmape=mean(abs((HWerr*100)/log_inv_test[1:length(log_inv_test)]))

HWmae
HWrmse
HWmape



arimaerr <- log_inv_test[1:length(log_inv_test)]- fcast$mean
arimamae=mean(abs(arimaerr))
arimarmse=sqrt(mean(arimaerr^2))
arimamape=mean(abs((arimaerr*100)/log_inv_test[1:length(log_inv_test)]))

arimamae
arimarmse
arimamape
