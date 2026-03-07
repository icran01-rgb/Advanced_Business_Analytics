# Logistic Regression Assignment
# 3/7/2026
# Ian Cran

# Load Libraries
library(readxl)
library(pscl)

data <- read.csv("C:/Users/iancr/OneDrive/Desktop/BA/admit.csv")

#EDA
nrow(data) #How many rows? - 400
ncol(data) #How many columns? - 4 
class(data) #What is the data clas? - data.frame
summary(data) #Descriptive Statistics 


#Are the classes (admit/ don't admit) balanced in the data set?
prop.table(table(data$admit)) #68.25% of the data are not admited, 31.75% are admited. 


#Plots
# How would you describe the distribution of GRE scores? Is it skewed or approximately normal?
hist(data$gre) #The data is skew left
hist(data$gpa) # The data is also skew left

# Logistic Regression Model
model <- glm(admit ~ gre + gpa + rank,
             data = data,
             family = "binomial") # Makes the regression model

summary(model) #Summary of model
# GPA is the best predictor, becasue it has the greatest effect on the log odds

#Intrepert Pseudo R Squared
pR2(model) #The pseudo R² is around 0.08, 
# indicating that the model explains about 8% of the variation in admission outcomes.

# Based on the predicitons and actual classes, how did your model do?
probabilities <- predict(model, type="response")
predictions <- ifelse(probabilities > 0.5, 1, 0)

table(Predicted=predictions, Actual=data$admit)

# What if the threshold was 0.8?
predictions80 <- ifelse(probabilities > 0.8, 1, 0)
table(Predicted=predictions80, Actual=data$admit)

