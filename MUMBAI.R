data = read.csv("5_TS_LEMON.csv")

#CHANGING TO TIME SERIES

MUMBAI <- ts(data$MUMBAI, start = c(2002, 1), end = c(2022, 12), frequency = 12)
MUMBAI

#PLOTTING DATA

plot(MUMBAI)


#ADF TEST TO CHECK P-VALUE

library(tseries)
adf.test(MUMBAI)

# Compute autocorrelation function
acf_result <- acf(MUMBAI)

# Plot autocorrelation function with title
plot(acf_result, main = "Mumbai")
# Compute autocorrelation function
pacf_result <- pacf(MUMBAI)

# Plot autocorrelation function with title
plot(pacf_result, main = "Mumbai")
# AUTO ARIMA

auto_arima =auto.arima(MUMBAI)

#FORECASTING FOR AUTO.ARIMA
library(forecast)
forecast = forecast(auto_arima,h=24)
forecast
autoplot(forecast,main = "Mumbai",xlab="Year", ylab = "Production in Tons")

#2022 VALUES(ACTUAL)

data_ts[240:252]

# AUTO.ARIMA IS THE BEST MODEL
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
write.csv(df, "MUMBAI.csv")