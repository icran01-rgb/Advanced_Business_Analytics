# In Class Assignment 
# 3/5/2026
# Ian Cran

# Load Libraries
library(readxl)
library(pscl)

data <- read.csv("C:/Users/iancr/OneDrive/Desktop/BA/Smoker.csv")

#EDA
nrow(data) #159256 Rows
ncol(data) #8 columns
class(data) #data.frame

#Plots
hist(data$age) #Data skew right, large number around 35-40
hist(data$height) #Data is normal
hist(data$weight) #Data is skew left, large cluster around 60-70
hist(data$relaxation) #Data is normal, with large cluster at around 70

# Descriptive Statistics
summary(data)

#Percent Smokers

# Logistic Regression Models
combined_model <- glm(smoking ~ age + height + weight + waist +
                        eyesight + systolic + relaxation,
                      data = data,
                      family = binomial)

summary(combined_model)
# For every 1-year increase in age, the log-odds of smoking decrease by 0.0368427.
# For every 1-unit increase in height, the log-odds of smoking increase by 0.1266.
# For every 1-unit increase in weight, the log-odds of smoking increase by 0.0631883.
# For every 1-unit increase in waist, the log-odds of smoking increase by 0.0633434.
# For every 1-unit increase in eyesight, the log-odds of smoking increase by 0.68103.
# For every 1-unit increase in systolic blood pressure, the log-odds of smoking increase by 0.0093061.
# For every 1-unit increase in relaxation (diastolic pressure), the log-odds of smoking increase by 0.024795.

coef(combined_model)
exp(coef(combined_model))

# Predicted the Probabilites
data$predicted_prob <- predict(combined_model, type="response") # predict the probability for each observation
data$predicted_prob # visualize probabilities

# Thresholds
# Threshold 0.5
data$predicted_05 <- ifelse(data$predicted_prob >= 0.5, 1, 0)

# Threshold 0.6
data$predicted_06 <- ifelse(data$predicted_prob >= 0.6, 1, 0)

# Threshold 0.9
data$predicted_09 <- ifelse(data$predicted_prob >= 0.9, 1, 0)

# Plot Sigmoid Curve for age
head(data)

plot(data$age, data$smoking,
     xlab = "Age",
     ylab = "Probability of Smoking")

curve(predict(age_model,
              data.frame(age = x),
              type = "response"),
      add = TRUE, col = "blue")

# Plot simgoid curve for waist
head(data)

plot(data$age, data$smoking,
     xlab = "Waist",
     ylab = "Probability of Smoking")

curve(predict(waist_model,
              data.frame(waist = x),
              type = "response"),
      add = TRUE, col = "blue")

# Plot sigmoid curve for systolic blood pressure
head(data)
plot(data$systolic, data$smoking,
     xlab = "Systolic Blood Pressure",
     ylab = "Probability of Smoking")

curve(predict(systolic_model,
              data.frame(systolic = x),
              type = "response"),
      add = TRUE, col = "blue")

# Generate and Intrepert the pseudeo-r squared
pR2(combined_model)

# The llh model is around -9.04, since the value is fairly negative, it shows the model is not very strong.
# The g^2 value being 3.74126e+04 shows a weak model