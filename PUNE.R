data = read.csv("5_TS_LEMON.csv")

#CHANGING TO TIME SERIES

PUNE <- ts(data$PUNE, start = c(2002, 1), end = c(2022, 12), frequency = 12)
PUNE

#PLOTTING DATA

plot(PUNE)


#ADF TEST TO CHECK P-VALUE

library(tseries)
adf.test(PUNE)

# Compute autocorrelation function
acf_result <- acf(PUNE)

# Plot autocorrelation function with title
plot(acf_result, main = "Pune")
# Compute autocorrelation function
pacf_result <- pacf(PUNE)

# Plot autocorrelation function with title
plot(pacf_result, main = "Pune")
# AUTO ARIMA

auto_arima =auto.arima(PUNE)

#FORECASTING FOR AUTO.ARIMA
library(forecast)
forecast = forecast(auto_arima,h=24)
forecast
autoplot(forecast,main = "Pune",xlab="Year", ylab = "Production in Tons")
#2022 VALUES(ACTUAL)

data_ts[240:252]

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
skewness <- skewness(PUNE)

# Compute kurtosis
kurtosis <- kurtosis(PUNE)

# Compute standard deviation
std_dev <- sd(PUNE)
skewness
kurtosis
std_dev
summary(PUNE)
df<- data.frame(forecast)
write.csv(df, "PUNE.csv")
