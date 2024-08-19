data = read.csv("5_TS_LEMON.csv")

#CHANGING TO TIME SERIES

CHANDIGARH <- ts(data$CHANDIGARH, start = c(2002, 1), end = c(2022, 12), frequency = 12)
CHANDIGARH
CHANDIGARH <- ts(data$CHANDIGARH, start = c(2002, 1), end = c(2020, 12), frequency = 12)

#PLOTTING DATA

plot(CHANDIGARH)

#ADF TEST TO CHECK P-VALUE

library(tseries)
adf.test(CHANDIGARH)


#DIFFERENCING FOR STATIONARITY
diff_ts = diff(CHANDIGARH)

#AGAIN TEST
adf.test(diff_ts)

# Compute autocorrelation function
acf_result <- acf(diff_ts)

# Plot autocorrelation function with title
plot(acf_result, main = "Chandigarh")
# Compute autocorrelation function
pacf_result <- pacf(diff_ts)

# Plot autocorrelation function with title
plot(pacf_result, main = "Chandigarh")
# AUTO ARIMA

auto_arima =auto.arima(data_ts)

#FORECASTING FOR AUTO.ARIMA
library(forecast)
forecast = forecast(auto_arima,h=24)
forecast
plot(forecast)
#2022 VALUES(ACTUAL)

data_ts[240:252]

# FITTING OWN ARIMA MODEL BY USING ACF AND PACF PLOTS

library(forecast)
model_2= Arima(CHANDIGARH,order =c(2,0,1))

#FORECASTING FOR FITTED ARIMA MODELS

forecast_2 = forecast(model_2,h=24)

forecast_2
#ACTUAL VALUES

data_ts[229:252]

#PLOTTING FORECAST

autoplot(forecast_2,main = "Chandigarh",xlab="Year", ylab = "Production in Tons")

#checking accuracy for every model

# For auto ARIMA
mae_auto = mean(abs(forecast$residuals))
mse_auto = mean(forecast$residuals^2)
rmse_auto = sqrt(mse_auto)
actual_values = data_ts[229:252]
mape_auto = mean(abs(forecast$residuals / actual_values)) * 100
mae_auto
mse_auto
rmse_auto
mape_auto


#for forecast_2
mae_2 = mean(abs(forecast_2$residuals))

mse_2 = mean(forecast_2$residuals^2)

rmse_2 = sqrt(mse_2)

mape_2= mean(abs(forecast_2$residuals / actual_values)) * 100
mae_2
mse_2
rmse_2
mape_2

# model 2 performing better


#install.packages("e1071")
library(e1071)
library(stats)
# Compute skewness
skewness <- skewness(data_ts)

# Compute kurtosis
kurtosis <- kurtosis(data_ts)

# Compute standard deviation
std_dev <- sd(data_ts)
skewness
kurtosis
std_dev
summary(data_ts)

df<- data.frame(forecast_2)
write.csv(df, "CHANDIGARH.csv")
