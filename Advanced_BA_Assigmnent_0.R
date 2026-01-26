# Ian Cran
# Advanced Busniness Analytics Assignment 0

getwd()
setwd("C:\\Users\\iancr\\OneDrive\\Desktop\\BA")

install.packages("readxl")
library(readxl)
data <- read.csv("netflix_titles.csv")

View(data)      # view data
head(data)      # view the first 6 rows
str(data)       # structure of the data
summary(data)   # summary statistics
colnames(data)  # display column names

# Above are a few basic explanitory data analysis that I have ran on the netflix titles data set.
# The view function allows us to view the entire data set. The head function allows us to view a 
# snippet of the data. The str function allows us to view the structure of the data set. The summary 
# function allows us to see summart statistics of the data set. Lastly, the colnames function 
# allows us to see the names of each column in the dataset.
