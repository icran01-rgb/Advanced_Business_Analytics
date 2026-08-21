# Load required libraries
install.packages("Metrics")
library(readxl)
library(forecast)
library(zoo)
library(readr)
library(Metrics)

# Read the CSV file
beer <- read_excel("monthly-beer-production-in-austr.xlsx")
#https://github.com/sergiomorapardo/Time-Series-Analysis-ARIMA-SARIMA-Prophet-LSTM/blob/master/dataset/datasets_56102_107707_monthly-beer-production-in-austr.csv

# Inspect the data
str(beer)
head(beer)

# Create time series object
beer_ts <- ts(
  beer$production,
  start = c(1956, 1),
  frequency = 12
)

# Plot the time series
plot(beer_ts,
     main = "Monthly Australian Beer Production",
     ylab = "Beer Production",
     xlab = "Time")


# Classical decomposition
beer_decomp <- decompose(beer_ts)

# Plot decomposition
plot(beer_decomp)



# 1-period moving average
ma_1 <- rollmean(beer_ts, k = 1, fill = NA, align = "right")
ma_1

# k is the number of the past periods we use to focus a future time period
# for k=1, the moving average is the same as the Naive(baseline) method where we use,
# one past time period to forecast the future

# 2-period moving average
ma_2 <- rollmean(beer_ts, k = 2, fill = NA, align = "right")
ma_2

# 3-period moving average
ma_3 <- rollmean(beer_ts, k = 3, fill = NA, align = "right")
ma_3

# Plot moving averages
plot(tail(beer_ts,30),
     main = "Moving Average Smoothing (Beer Production)",
     ylab = "Production",
     xlab = "Time")
lines(ma_2, col = "blue")
lines(ma_3, col = "red")

legend("topleft",
       legend = c("Original", "2-period MA", "3-period MA"),
       col = c("black", "blue", "red"),
       lty = 1)


# One-step-ahead forecasts using latest MA values
ma1_forecast <- tail(ma_1, 1)
ma2_forecast <- tail(ma_2, 1)
ma3_forecast <- tail(ma_3, 1)

ma1_forecast


#Error Metrics
mse(ma2_forecast,beer_ts) #MSE
mse(ma3_forecast,beer_ts)

mae(ma2_forecast,beer_ts) #MAE
mape(ma2_forecast,beer_ts) #MAPE


# Fit exponential smoothing model
ets_model <- ets(beer_ts)

# Forecast next 12 months
ets_forecast <- forecast(ets_model, h = 12)

# Plot forecast
plot(ets_forecast)


summary(ets_model)

