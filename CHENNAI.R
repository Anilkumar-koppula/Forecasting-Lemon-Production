data = read.csv("5_TS_LEMON.csv")

#CHANGING TO TIME SERIES

CHENNAI <- ts(data$CHENNAI, start = c(2002, 1), end = c(2022, 12), frequency = 12)
CHENNAI

#PLOTTING DATA

plot(CHENNAI)
#ADF TEST TO CHECK P-VALUE

library(tseries)
adf.test(CHENNAI)

# Compute autocorrelation function
acf_result <- acf(CHENNAI)

# Plot autocorrelation function with title
plot(acf_result, main = "Chennai")
# Compute autocorrelation function
pacf_result <- pacf(CHENNAI)

# Plot autocorrelation function with title
plot(pacf_result, main = "Chennai")

# AUTO ARIMA

auto_arima =auto.arima(CHENNAI)

#FORECASTING FOR AUTO.ARIMA
library(forecast)
forecast = forecast(auto_arima,h=24)
forecast
autoplot(forecast)
autoplot(forecast,main = "Chennai",xlab="Year", ylab = "Production in Tons")


#2022 VALUES(ACTUAL)

data_ts[240:252]

#AUTO.ARIMA IS THE BEST WITH (1,1,1)(2,0,0)[12]
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

df<- data.frame(forecast)
write.csv(df, "CHENNAI.csv")
