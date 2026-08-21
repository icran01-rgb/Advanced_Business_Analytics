# Time Series 
# Ian Cran, Alex Her
# 4/28/2026

# Load libraries
library(readxl)
library(forecast)
library(zoo)
library(readr)
library(Metrics)
library(dplyr)
library(tseries)


data <- read_csv("C:/Users/iancr/OneDrive/Desktop/BA/monthly_production.csv")


# What is a time series? Give two real-world examples. 
# A time series dataset is a dataset with N sample units with only one variable, but
# recorded at T diferent time stamps. Real worlds examples include plotting trends in stock prices,
# also tracking historical weather data.

# What is the difference between a time series and cross-sectional data?
# The difference between a time series dataset and a cross-sectional dataset is that 
# in a time series dataset, you are recording only one varaible a different time stamps,
# in a cross-sectional dataset, you are recording multiple variables at a single point in time. 


# Sort the data in ascending order based on the month column.
data_sorted <- data[order(data$month), ]
print(data_sorted)

#Create time series objective
production_ts <- ts(
  data$production,
  start = c(1956, 1),
  frequency = 12
)

# Plot time series
# Plot the time series
plot(production_ts,
     main = "Monthly Production",
     ylab = "Production",
     xlab = "Time")
# There are a few noticable trends in this plot. The first one we noticed was a slight long-term positive trend from 1956 - 1970.
# We also noticed a leveling off and even slight decrease in the data beginnning in late 1970's.
# The final trend we noticed was strong seasonality, with large spikes and dips in a repeated, predicable pattern.

# Decompose the time series
# Classical decomposition
production_decomp <- decompose(production_ts)
# Plot decomposition
plot(production_decomp)
# This function in  R breaks the trend of the time series down into 4 readable parts. 
# Random, seasonal, trend, observed, we can see a upwards trend, as well as the sesonality we mentioned in the previous questions. 


# What is autocorrelation? Plot the autocorrelation function (ACF) and partial autocorrelation function (PACF) for the dataset.
# Autocorrelation measures the relationship between a time series and its own lags.

acf(production_ts, main = "Autocorrelation Function")
# This function shows the correlation of the series with different lags.

pacf(production_ts, main = "Partial Autocorrelation Function")
# This function shows the direct correlation between the current value and specific lags.

train_ts <- window(production_ts, end = c(1990,12))
test_ts  <- window(production_ts, start = c(1991,1))


# Split the time series into a training and test set. Why is this important?
# This is important because the training set = older historical data used to build the forecasting model,
# the test set = most recent observations used to evaluate how well the model predicts unseen future values
# The test set allows us to measure forecast accuracy, compare models, avoid overfitting, and see how the model performs on new data

# Perform a moving average smoothing on the data. What window size did you choose and why?
# 3-period moving average
ma_3 <- rollmean(production_ts, k = 3, fill = NA, align = "right")
ma_3

# We chose a 3 period rolling mean because it allows the jagged seasonalitity to remain, 
# while allowing us to see short-term fluctuations

# Fit a simple exponential smoothing model to the series. Evaluate its performance.
ets_model <- ets(production_ts)
# The model updates the level slowly beacuse of the small alpha of 0.0786
# The model shows long-term trend changes gradually over time with a small beta of 0.0086
# The gamma level is 0.0001, meaning the seasonal pattern is very stable and repeats similarly year after year.
# The small sigma of 0.0683 means the ETS model fits the data fairly well.


# Forecast the next 5 periods. Plot the forecast along with confidence intervals.

# Forecast next 5 periods
ets_forecast <- forecast(ets_model, h = 5)

# Plot the forecast
plot(ets_forecast,
     main="5-Period Forecast Using ETS Model",
     ylab="Production")

summary(ets_forecast)
# This function allows us to see the confidence intervals.

# Compare the forecasting accuracy using metrics like MAE, RMSE, and MAPE.
ma3_forecast <- tail(ma_3, 1)

#Error Metrics
mse(ma3_forecast,production_ts) #MSE, 361
mae(ma3_forecast,production_ts) #MAE, 19
mape(ma3_forecast,production_ts) #MAPE, 0.141791

#MSE = 361, This is the average squared error

#MAE = 19, On average, our forecasts are off by about 19 units from the actual values.

#MAPE = 0.1418, On average, our forecasts are about 14% off from actual values.

# How would you deal with missing values in a time series?
# There are a few ways we can deal with missing values, we can delete the missing values, use seasonal trends to fill in the missing values,
# or backwards and forwards fills, which is using the closest past or future value to fill. 


# Perform a stationarity test (e.g., Augmented Dickey-Fuller test) on the data. Interpret the results.
adf.test(production_ts)

# Since the p-value is less that 0.5, we reject the null hypothesis. 
# Our time series is stationary
# There is no differencing needed

#Ensemble the output of the two models and find the MAE, RMSE, and MAPE of the ensemble model

# Model 1
ma3_model <- Arima(production_ts, order = c(0,0,3))
ma3_forecast <- forecast(ma3_model, h = 5)
ma3_values <- as.numeric(ma3_forecast$mean)

# We created the first model which is the moving average 3 value we predicted previously. 

# Model 2
ets_model <- ets(production_ts)
ets_forecast <- forecast(ets_model, h = 5)
ets_values <- as.numeric(ets_forecast$mean)

# We created the second model which was the exponential smoothing model that we also predicted prevosuly. 
 
# Actual values (last 5 observations)
actual <- as.numeric(tail(production_ts, 5))


# Ensemble forecast (simple average)
ensemble <- (ma3_values + ets_values) / 2

# We combine both models by taking a simple average
# This helps reduce individual model error


#Accuracy metrics
mae_ens <- mean(abs(actual - ensemble))
rmse_ens <- sqrt(mean((actual - ensemble)^2))
mape_ens <- mean(abs((actual - ensemble) / actual))

# These measure how close the ensemble predictions are to actual values


# Print results
mae_ens
rmse_ens
mape_ens
# Print final evaluation metrics for ensemble model

