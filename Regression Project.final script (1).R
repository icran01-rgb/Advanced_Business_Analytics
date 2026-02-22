# Week 3 Regression Project - Ian Cran & Alex Her

# libraries 
library(ggplot2)
library(tidyverse)
library(readxl)
library(rlang)
library(lmtest)
library(car)

# To Load & view dataset
GPA.data <- read_excel("C:/Users/alexh/Downloads/GPA.xlsx")

# Check dataset
sum(is.na(GPA.data)) #0 NA's
view(GPA.data)
summary(GPA.data)
str(GPA.data) #all numeric

# Removes all 0 values in Cumulative GPA
Filtered_GPA_data <- GPA.data %>%
  filter(cumgpa != 0, !is.na(cumgpa))

# Convert categorical columns to factors
Filtered_GPA_data$season <- as.factor(Filtered_GPA_data$season)
Filtered_GPA_data$frstsem <- as.factor(Filtered_GPA_data$frstsem)
Filtered_GPA_data$female <- as.factor(Filtered_GPA_data$female)
Filtered_GPA_data$black <- as.factor(Filtered_GPA_data$black)
Filtered_GPA_data$white <- as.factor(Filtered_GPA_data$white)
Filtered_GPA_data$football <- as.factor(Filtered_GPA_data$football)

# Check for columns with only 1 value
sapply(Filtered_GPA_data, function(x) length(unique(x)))

# because frstsem has one 1 value, remove it from the data set
Master.GPA.data <- Filtered_GPA_data %>% select(-frstsem)

# Check filtered data
view(Master.GPA.data)
str(Master.GPA.data)
summary(Master.GPA.data)

# Create a regression of everything
Everything_reg <- lm(cumgpa ~ ., data = Master.GPA.data)
summary(Everything_reg)

# Create a regression of only significant variables
signif_reg <- lm(cumgpa ~ sat + tothrs + trmgpa + female + football, data = Master.GPA.data)
summary(signif_reg)

# Best fit model
best_fit_model <- lm(cumgpa ~ trmgpa * sat + trmgpa^2 + tothrs + female + football, data = Master.GPA.data)
summary(best_fit_model)

# test for linearity with basic residual vs fitted plot
plot(test_reg, which = 1)

# test for heteroskedasticity
bptest(best_fit_model) # P-value < 0.05, we reject null, heteroskedasticity is present

# test for independence of errors
dwtest(best_fit_model) #dw is close to 2, no autocorrelation

# test for normality
plot(best_fit_model, which = 2)
shapiro.test(residuals(test_reg))

# Test for Multicollinearity
vif(signif_reg) # used signif_reg, because the variables are squared or interacted yet


# Plot of Cumulative GPA
ggplot(Filtered_GPA_data, aes(x = cumgpa)) +
  geom_histogram(binwidth = 0.25, fill = "azure3", color = "black") +
  stat_bin(binwidth = 0.25, geom = "text",
           aes(label = after_stat(count)),
           vjust = -0.3, size = 4) +
  labs(title = "Distribution of Cumulative GPA",
       x = "Cumulative GPA",
       y = "Frequency") + 
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(breaks = seq(0,4,0.5)) + 
  theme_classic()

# Simple Linear model CumGPA vs SAT
ggplot(Master.GPA.data, aes(x = sat, y = cumgpa)) +
  geom_point(size = 2) +  # scatter plot
  geom_smooth(method = "lm", color = "blue", formula = y ~ x, se = TRUE) +  # linear fit with CI
  scale_x_continuous(breaks = seq(400, 1600, 100)) +  # SAT axis: 400-1600, by 100
  scale_y_continuous(breaks = seq(0, 4, 0.5)) +       # GPA axis: 0-4, by 0.5
  labs(
    title = "Cumulative GPA vs SAT",
    subtitle = "Blue = Linear fit (with 95% CI)",
    x = "SAT Score",
    y = "Cumulative GPA"
  ) +
  theme_minimal()

sat_reg <- lm(cumgpa ~ sat, data = Master.GPA.data)
summary(sat_reg)

# Simple Linear model CumGPA vs trmGPA
ggplot(Master.GPA.data, aes(x = trmgpa, y = cumgpa)) +
  geom_point(size = 2) +  # scatter plot
  geom_smooth(method = "lm", color = "blue", formula = y ~ x, se = TRUE) +  # linear fit with CI
  geom_smooth(method = "lm", color = "red", formula = y ~ poly(x, 2), se = TRUE) +  # quadratic fit with CI
  scale_x_continuous(breaks = seq(0, 4, 0.5)) +  # GPA axis: 0-4, by 0.5
  scale_y_continuous(breaks = seq(0, 4, 0.5)) +       # GPA axis: 0-4, by 0.5
  labs(
    title = "Cumulative GPA vs Term GPA",
    subtitle = "Blue = Linear fit, Red = Quadratic fit (with 95% CI)",
    x = "Term GPA",
    y = "Cumulative GPA"
  ) +
  theme_minimal()

trmgpa_reg <- lm(cumgpa ~ trmgpa, data = Master.GPA.data)
summary(trmgpa_reg)

# Simple Linear model CumGPA vs Tothrs
ggplot(Master.GPA.data, aes(x = tothrs, y = cumgpa)) +
  geom_point(size = 2) +  # scatter plot
  geom_smooth(method = "lm", color = "blue", formula = y ~ x, se = TRUE) +  # linear fit with CI
  scale_x_continuous(breaks = seq(1,121,10)) +  # credit hours 1-121, by 10
  scale_y_continuous(breaks = seq(0, 4, 0.5)) +       # GPA axis: 0-4, by 0.5
  labs(
    title = "Cumulative GPA vs Total Credit Hours",
    subtitle = "Blue = Linear fit (with 95% CI)",
    x = "Total Credit Hours",
    y = "Cumulative GPA"
  ) +
  theme_minimal()


tothrs_reg <- lm(cumgpa ~ tothrs, data = Master.GPA.data)
summary(tothrs_reg)




# Linear Fit model: SAT interacts with term GPA, and term GPA squared
# Representative term GPA values
trmgpa_vals <- quantile(Master.GPA.data$trmgpa, probs = c(0.25, 0.5, 0.75))

# Prediction data frame
pred_data <- expand.grid(
  sat = seq(400, 1600, 50),       # SAT scores
  trmgpa = trmgpa_vals,
  tothrs = mean(Master.GPA.data$tothrs),  # hold others constant
  female = 0,
  football = 0
)

# Add predicted cumgpa
pred_data$pred <- predict(best_fit_model, newdata = pred_data)

ggplot() +
  geom_point(data = Master.GPA.data, aes(x = sat, y = cumgpa), alpha = 0.5) +
  geom_line(data = pred_data, aes(x = sat, y = pred, color = factor(trmgpa)), size = 1.2) +
  scale_color_manual(values = c("blue", "green", "red"),
                     labels = paste("trmgpa =", round(trmgpa_vals, 2)),
                     name = "Term GPA") +
  scale_x_continuous(breaks = seq(400, 1600, 100)) +
  scale_y_continuous(breaks = seq(0, 4, 0.5)) +
  labs(title = "Cumulative GPA vs SAT by Term GPA (Quadratic)",
       subtitle = "Lines show predicted GPA at low, medium, high term GPA",
       x = "SAT Score",
       y = "Cumulative GPA") +
  theme_minimal()
