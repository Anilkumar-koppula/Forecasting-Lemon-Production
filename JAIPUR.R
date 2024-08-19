data = read.csv("5_TS_LEMON.csv")

#CHANGING TO TIME SERIES

JAIPUR <- ts(data$JAIPUR, start = c(2002, 1), end = c(2022, 12), frequency = 12)
JAIPUR

#PLOTTING DATA

plot(JAIPUR)


#ADF TEST TO CHECK P-VALUE

library(tseries)
adf.test(JAIPUR)

#ACF & PACF
# Compute autocorrelation function
acf_result <- acf(JAIPUR)

# Plot autocorrelation function with title
plot(acf_result, main = "Jaipur")
# Compute autocorrelation function
pacf_result <- pacf(JAIPUR)

# Plot autocorrelation function with title
plot(pacf_result, main = "Jaipur")

# AUTO ARIMA

auto_arima =auto.arima(JAIPUR)

#FORECASTING FOR AUTO.ARIMA
library(forecast)
forecast = forecast(auto_arima,h=24)
forecast
autoplot(forecast,main = "Jaipur",xlab="Year", ylab = "Production in Tons")

#2022 VALUES(ACTUAL)

JAIPUR[240:252]

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
#auto arima is the best fit



#install.packages("e1071")
library(e1071)
library(stats)
        
# Compute skewness
skewness <- skewness(JAIPUR)

# Compute kurtosis
kurtosis <- kurtosis(JAIPUR)

# Compute standard deviation
std_dev <- sd(JAIPUR)
skewness
kurtosis
std_dev
summary(data_ts)
#df<- data.frame(forecast)
#write.csv(df, "JAIPUR.csv")
