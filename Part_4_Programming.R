#loading CART decision tree from part 3
load("datamodel.CART")


#Using the predict function to apply to the model that is part of my classifier
pred.tree <- predict(tree, newdata = train, type = "class")

#using the mean function to get the average of those who survived, using the CART tree
mean(pred.tree==train$Survived)
pred.table <- table(pred.tree, train$Survived)
pred.table

#After analyzing the CART decision tree, we can conclude the following.
#The passenger that is most likely to survive the titanic will had been a female whose age was 37 or older, not matter what ticket class she was in.
#From our table, the average survival for this demographic of passenger was 0.83, not to far off from the 0.76 we discovered in part 2.

save(pred.tree, pred.table, file = "finalproj.CART")
