#loading objects from part 1
load("dataprep.proj")

#EDA for variables "Sex" and "Survived"
#creating a table for sex and survived
t1.survived.sex <- table(train$Survived, train$Sex)
t1.sum <- addmargins(t1.survived.sex, FUN = sum)
t1.sum

barplot(t1.survived.sex,
        legend = rownames(t1.survived.sex),
        ylim = c(0,800),
        main = "Bar Graph of Survivers and Sex",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")
#Although there were more men onboard the titanic, they were more likely to not survive.


#EDA for variables "Survived" and "Age"
summary(train$Age)
#Average age is 29.36
#Median age is 28.00

#filtering for females under the age of 18
train_female_0.18 <- train %>%
  select(Age, Sex, Survived) %>%
  filter(Sex == "female" & Age < 18.00)
summary(train_female_0.18)
#The average of survival is 0.69

#creating a table for females under the age of 18
t2.age.female_0.18 <- table(train_female_0.18$Survived, train_female_0.18$Age)
t2.age.female_0.18

barplot(t2.age.women_0.18,
        legend = rownames(t2.age.women_0.18),
        ylim = c(0,10),
        ylab = "Count",
        xlab = "Age",
        main = "Survival of Females Under the Age of 18",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")

#filtering for females over the age of 18
train_female_18.plus <- train %>%
  select(Age, Sex, Survived) %>%
  filter(Sex == "female" & Age > 18.00)
summary(train_female_18.plus)
#the average of survival is 0.76

#creating a plot for females over the age of 18
t3.age_female_18.plus <- table(train_female_18.plus$Survived, train_female_18.plus$Age)
barplot(t3.age_female_18.plus,
        legend = rownames(t3.age_female_18.plus),
        ylim = c(0,70),
        ylab = "Count",
        xlab = "Age",
        main = "Survival of Females Over the Age of 18",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")

#Comparing both age groups of women, we see that the survival average for women over the age of 18 is higher than those under the age of 18.
#It is important to note that there were more women over the age of 18 that were on the titanic, thus being the reason why the average is higher. 

#filtering for male under the age of 18
train_male_0.18 <- train %>%
  select(Age, Sex, Survived) %>%
  filter(Sex == "male" & Age < 18.00)
summary(train_male_0.18)
#the average of surivial is 0.3966

#creating a plot for male between the age 0 & 18
t4.age_male_0.18 <- table(train_male_0.18$Survived, train_male_0.18$Age)
barplot(t4.age_male_0.18,
        legend = rownames(t4.age_male_0.18),
        ylim = c(0,20),
        ylab = "Count",
        xlab = "Age",
        main = "Survival of Males Under the Age of 18",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")

#filtering for male over the age of 18
train_male_18.plus <- train %>%
  select(Age, Sex, Survived) %>%
  filter(Sex == "male" & Age > 18.00)
summary(train_male_18.plus)
#the average of surivial is 0.168

#creating a plot for men over the age of 18
t5.age_male_18.plus <- table(train_male_18.plus$Survived, train_male_18.plus$Age)
barplot(t5.age_male_18.plus,
        legend = rownames(t5.age_male_18.plus),
        ylim = c(0,150),
        ylab = "Count",
        xlab = "Age",
        main = "Survival of Males Over the Age of 18",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")

#Comparing my analysis of males under the age of 18 and over the age of 18, males under the age of 18 had a higher average of survival.
#Out of all four subsets of age groups and sex, it is evident that males over the age of 18 had the highest rate of not surviving.

#EDA for variables "Survived" and "Pclass"
t6.survived.pclass <- table(train$Survived, train$Pclass)
t6.survived.pclass
t6.sum <- addmargins(t6.survived.pclass, FUN = sum)
t6.sum
#By observing this table, we can see that those in 1st class survived more than those in any other classes.

#creating a barplot for pclass and survived
barplot(t6.survived.pclass,
        legend = rownames(t6.survived.pclass),
        ylim = c(0,600),
        main = "Bar Graph of Survivers and Ticket Class",
        ylab = "Count",
        xlab = "Ticket Class",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")

#Observation of the barplot agrees with my suspicion that those in 1st class had a higher chance of survival.

#EDA of'Survived' and 'SibSp'
t7.survived.sibsp <- table(train$Survived, train$SibSp)

#creating a barplot for survived and SibSp
barplot(t7.survived.sibsp,
        legend = rownames(t7.survived.sibsp),
        ylim = c(0,700),
        main = "Bar Graph of Survivers & Number of Children and Spouses",
        ylab = "Count",
        xlab = "# of Children and Spouses",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")
#Those with no children or spouses onboard had the highest chances of survival.

#EDA of 'Survived' and 'Parch'
t8.survived.parch <- table(train$Survived, train$Parch)

#creating a barplot for 'Survived' and 'Parch'
barplot(t8.survived.parch,
        legend = rownames(t8.survived.parch),
        ylim = c(0,800),
        main = "Bar Graph of Survivers and Those With Parents and Children",
        ylab = "Count",
        xlab = "# of Parents and Children",
        col = c("blue", "red"))
box(which = "plot",
    lty = "solid",
    col = "black")
#Similarly to my analysis of Children and Spouses, those with no parents and children on board had the highest chances of survival. 

model <- glm(formula = Survived ~., family = binomial(link = "logit"), data = train)
summary(model)
#From analyzing this summary, we observed that Fare, Parch, and Age are not statisitcally significant.


#Clues that were found
#Females were more likely to survive than males
#From the average, females over the age of 18 have the highest chance of survival.
#Whereas males over the age of 18 had the least chance of survival.
#Also, those who have first class tickets have a higher chance of survival than those with second and third class tickets.
#And those without siblings, children, or parents have a higher chance of survival. 

