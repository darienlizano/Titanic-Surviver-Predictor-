#installing packages for data preparation
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")

#loading packages
library(dplyr)
library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)

#loading dataset
train <- read_csv("C:/Users/Darien/Downloads/train.csv")
View(train)


#removing 'PassengerId', 'Ticket', 'Name',  and 'Cabin' columns. 
#'PassengerId' is directly attached to a specific passenger, therefor it is not needed in my analysis.
#'Cabin" has too many missing values. The textbook calls for only 5% of missing data, whereas 'Cabin' clearly is missing more than 5%.
#'Ticket' and 'Name' are tied specifically to each passenger. These values are not useful for my analysis.
train <- train[-c(1, 4, 9, 11)]

#checking for NA's
is.na(train)
train[!complete.cases(train),]

#replacing NA values in 'Age' with median age
train <-  train %>% mutate(Age=ifelse(is.na(Age), median(Age, na.rm=TRUE), Age))

#replacing NA values in 'Embarked' with random value
random_embark <- sample(na.omit(train$Embarked), 1)
train[62, 8] <- random_embark
train[830, 8] <- random_embark
train[!complete.cases(train),]

#converting categorical data into numerics
train$Sex[train$Sex == "female"] <- 0
train$Sex[train$Sex == "male"] <- 1

train$Embarked[train$Embarked == "C"] <- 0
train$Embarked[train$Embarked == "Q"] <- 1
train$Embarked[train$Embarked == "S"] <- 2

#converting numerical data inter integers
train$Sex <- as.integer(train$Sex)
train$Embarked <- as.integer(train$Embarked)

class(train$Embarked)
class(train$Sex)

#identifying outliers with a histogram of passenger age
hist(train$Age,
     breaks = 20,
     xlim = c(0,80),
     col = "blue",
     ylim = c(0,300),
     xlab = "Age",
     ylab = "Counts",
     main = "Histogram of Passenger Ages on Titanic")
box(which = "plot",
    lty = "solid",
    col = "black")
#no outliers. Most passengers were under the age of 40. 

#identifying outliers with a histogram of passenger fare
hist(train$Fare,
     breaks = 10,
     xlim = c(0,500),
     col = "blue",
     ylim = c(0,800),
     xlab = "Fare",
     ylab = "Counts",
     main = "Histogram of Passenger Fare on Titanic")
box(which = "plot",
    lty = "solid",
    col = "black")
#There is an outlier for fare, exceed 500

#identifying outliers with a histogram showing number of siblings and spouses on the titantic
hist(train$SibSp,
     breaks = 20,
     xlim = c(0,8),
     col = "blue",
     ylim = c(0,800),
     xlab = "Spouses & Siblings",
     ylab = "Counts",
     main = "Histogram of Spouses and Siblings on Titanic")
box(which = "plot",
    lty = "solid",
    col = "black")
#there is an outlier, 8 Spouses and/or siblings

#identifying outliers with a histogram of ticket class
hist(train$Pclass,
     breaks = 20,
     xlim = c(1.0,3.0),
     col = "blue",
     ylim = c(0,600),
     xlab = "Ticket Class",
     ylab = "Counts",
     main = "Histogram of Ticket Class on Titanic")
box(which = "plot",
    lty = "solid",
    col = "black")
#no outlier, since there are 3 classes of tickets that were sold

#identifying outliers with a histogram showing number of parents and children on the titanic
hist(train$Parch,
     breaks = 20,
     xlim = c(0,6),
     col = "blue",
     ylim = c(0,800),
     xlab = "Parents & Children",
     ylab = "Counts",
     main = "Histogram of Parents and Children on Titanic")
box(which = "plot",
    lty = "solid",
    col = "black")
#no outliers

save(train, random_embark, file = "dataprep.proj")

