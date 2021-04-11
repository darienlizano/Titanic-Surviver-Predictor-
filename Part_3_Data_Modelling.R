#loading objects from part 1
load("dataprep.proj")

#loading readr and dataset
library(readr)
test <- read_csv("C:/Users/Darien/Downloads/test.csv")
View(test)

#removing 'PassengerId', 'Ticket', 'Name', and 'Cabin' columns. 
test <- test[-c(1, 3, 8, 10)]
is.na(test)

#checking for NA's
is.na(test)
test[!complete.cases(test),]

#replacing NA values in 'Age' with median age and 'Fare" with median fare
test <-  test %>% mutate(Age=ifelse(is.na(Age), median(Age, na.rm=TRUE), Age)) %>%
  mutate(Fare=ifelse(is.na(Fare), median(Fare, na.rm=TRUE), Fare))

#replacing NA values in 'Embarked' with random value
test_random_embark <- sample(na.omit(test$Embarked), 1)
test[!complete.cases(test),]

#converting categorical data into numerics
test$Sex[test$Sex == "female"] <- 0
test$Sex[test$Sex == "male"] <- 1

test$Embarked[test$Embarked == "C"] <- 0
test$Embarked[test$Embarked == "Q"] <- 1
test$Embarked[test$Embarked == "S"] <- 2

#converting numerical data into integers
test$Sex <- as.integer(test$Sex)
test$Embarked <- as.integer(test$Embarked)

class(test$Embarked)
class(test$Sex)

#ensuring numerical data is numerical
class(test$Pclass)
class(test$Age)
class(test$SibSp)
class(test$Parch)
class(test$Fare)

#combining test and train to one dataset
#reason for doing so is to include my target variable 'Survived'
#Excluding this variable will disallow me to further my analysis
titanic <- bind_rows(train, test)
titanic <- na.omit(titanic)
View(titanic)
titanic[!complete.cases(titanic),]


#normalize data
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

test[, c(3, 6)] <- lapply(test[, c(3, 6)], normalize)


#Applying classification methods

#K-nearest neighbor
#install package for KNN
install.packages("class")
library(class)
library(tidyverse)

#creating KNN algorithm
train_knn <- titanic[2:891,]
test_knn <- titanic[1,]
trueclass <- titanic[2:891, 1]
trueclass <- as.vector(trueclass)
test_knn <- knn(train_knn, test_knn, cl = trueclass$Survived, k = 3)
test_knn


#Decision Trees
#installing and loading package for decision tree
install.packages(c("rpart", "rpart.plot", "C50"))
library(rpart)
library(rpart.plot)
library(C50)

#Generating CART decision tree
set.seed(123)
tree <- rpart(Survived ~., data = titanic, method = "class")
rpart.plot(tree)

#Generating a C4.5/C5.0 decision tree
titanic$Survived <- as.factor(titanic$Survived)
c50fit <- C5.0(`Survived` ~., data = titanic)
plot(c50fit)

#From these three models, the CART decision tree is the best model to use for this task.
#It is apparent that ther CART decision tree yielded much more information that allows us to predict who mauy have survived the Titanitc.
#The KNN and C4.5/C5.0 algoritms proved to be functional, but lack the information needed for this task.


#saving tree object for part 4
save(tree, file = "datamodel.CART")




