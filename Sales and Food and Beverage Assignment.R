# Advanced Business Analytics Time Series and Forecasting Assignment
# Ian Cran

# Libraries
library(readxl)
library(forecast)
library(zoo)
library(readr)
library(Metrics)

sales <- read_excel("Sales.xlsx")
food_and_beverage <- read_excel("Food_and_Beverage.xlsx")

# Create time series objective
sales_ts <- ts(
  sales$Sales,
  start = c(29, 1),
  frequency = 1
)

# Time Series Plot
plot(sales_ts,
     main = "Weekely Sales",
     ylab = "Sales",
     xlab = "Week")


# Rolling average
ma_3 <- rollmean(sales_ts, k=3, fill = NA, align = "right")
ma_3 # rolling average of 3

ma_4 <- rollmean(sales_ts, k=4, fill = NA, align = "right")
ma_4 # rolling average of 4

ma_5 <- rollmean(sales_ts, k=5, fill = NA, align = "right")
ma_5 # rolling average of 5


# Exponential Smoothing
es_model <- ses(sales_ts, alpha = 0.5, initial = "simple")
es_forecast <- forecast(es_model, h = 1)
es_forecast

# Best Fit 
ma3_forecast <- tail(ma_3, 1)
ma4_forecast <- tail(ma_4, 1)
ma5_forecast <- tail(ma_5, 1)


mse(ma3_forecast,sales_ts) #MSE - 0.1111111
mae(ma3_forecast,sales_ts) #MAE - 0.3333333
mape(ma3_forecast,sales_ts) #MAPE - 0.01923077


mse(ma4_forecast,sales_ts) #MSE - 1
mae(ma4_forecast,sales_ts) #MAE - 1
mape(ma4_forecast,sales_ts) #MAPE - 0.05555556

mse(ma5_forecast,sales_ts) #MSE - 1.96
mae(ma5_forecast,sales_ts) #MAE - 1.4
mape(ma5_forecast,sales_ts) #MAPE - 0.07608696

mse_es <- mean(residuals(es_model)^2, na.rm = TRUE)# MSE 9.169246
mae_es <- mean(abs(residuals(es_model)), na.rm = TRUE) # MAE 2.468611
mape_es <- mean(abs(residuals(es_model) / sales_ts), na.rm = TRUE) * 100 # MAPE 12.73785

# Alpha that minimizes MSE

es_opt <- ses(sales_ts, initial = "simple")
es_opt$model$par
# 0.323883 

# Best Model
# The model that best fit the forecast, was the MAPE 5 forecast as the output was the smallest value.

# Case Study
sales_vector <- c(
  food_and_beverage$`First Year`,
  food_and_beverage$`Second Year`,
  food_and_beverage$`Third Year`
) # Sales Vector

# Time series objective
sales_ts <- ts(
  sales_vector,
  start = c(1, 1),   # Year 1, January
  frequency = 12     # Monthly data
)

sales_ts

plot(sales_ts, 
     main = "Food and Beverage Sales",
     xlab = "Year",
     ylab = "Sales"
     )
# There appears to be a sesonality, with spikes towards the end of the year.

time <- 1:length(sales_ts)
month <- factor(rep(1:12, length.out = length(sales_ts)))

sales_df <- data.frame(
  sales = as.numeric(sales_ts),
  time = time,
  month = month
)

# Dummy Model
dummy_model <- lm(sales ~ time + month, data = sales_df)
summary(dummy_model)

# Create year 4
future_df <- data.frame(
  time = 37:48,
  month = factor(1:12, levels = 1:12)
)

# Year 4 forecast
year4_forecast <- predict(dummy_model, newdata = future_df)
year4_forecast
data.frame(
  Month = c("January","February","March","April","May","June",
            "July","August","September","October","November","December"),
  Forecast_Year4 = round(year4_forecast, 2)
)

# The model follows previous trends of the data. Sales are slowly rising for
# each month year to year. As well as an increase in sales in december. 

# If sales in janurary turn out to be 295000, the forecast error is 7,420
# Use MSE, MAE, and MAPE to find the margin of error of our forecast, to show that our forecast in accurate withing a margin.