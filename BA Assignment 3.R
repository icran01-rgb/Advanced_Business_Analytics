install.packages("readxl") 
library(readxl)

wages <- read_excel("wages.xlsx") 
rent <- read_excel("AnnArbor.xlsx")


plot(wages$Age, wages$Wage,
     main = "Wage vs Age",
     xlab = "Age",
     ylab = "Wage") # Since there is a curve in the data, a quadratic model is better


linear_model <- lm(Wage ~ Age + Educ, data = wages)
summary(linear_model)

quadratic_model <- lm(Wage ~ Age + Educ + I(Age^2), data = wages)
summary(quadratic_model)

# The R-Sqaured for a linear model is 0.6088, while the r squared for the quadratic model
# is higher at 0.8257, meaning the quadratic is better

prediction_data <- data.frame(
  Age = c(30, 50, 70),
  Educ = c(16, 16, 16)
)

predict(quadratic_model, prediction_data)

# According to the prediction, a person with 16 years of education will make the most wages at 50 years old

plot(rent$Beds, rent$Rent,
      main = "Rent vs Bedrooms",
      xlab = "Bedrooms",
      ylab = "Rent")

plot(rent$Baths, rent$Rent,
     main = "Rent vs Bathrooms",
     xlab = "Bathrooms",
     ylab = "Rent")

plot(rent$Sqft, log(rent$Rent),
     main = "log(Rent) vs SqFt",
     xlab = "Square Footage",
     ylab = "log(Rent)")

rent_model <- lm(log(Rent) ~ Beds + Baths + Sqft, data = rent)
summary(rent_model)
# Bedrooms and Bathrooms are linear, while rent and SqFt are likely non linear, making log transofrmations better.

new_rental <- data.frame(
  Beds = 3,
  Baths = 2,
  Sqft = 1600
)

log_rent_pred <- predict(rent_model, newdata = new_rental)
rent_pred <- exp(log_rent_pred)
rent_pred
# 1447.295 


