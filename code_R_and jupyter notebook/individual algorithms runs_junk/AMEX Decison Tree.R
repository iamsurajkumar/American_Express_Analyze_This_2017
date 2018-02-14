# Checking the Data
data <- mydata_G
str(data)
names(mydata_G)


# Library loading
library(tree)
mytree = tree(Offer.Accepted ~ fam.size + account.age + plcard + freq.payments + Customer.Response + total.spend ,mydata_G)
mytree
summary(mytree)

mytree2 <- ctree(Offer.Accepted ~ . - cm_key, data = mydata_G)

mytree_short <- ctree(Offer.Accepted ~ . - cm_key, data = mydata_G,controls = ctree_control(mincriterion = 0.99, minsplit = 500))




plot(mytree_short)
text(mytree_short,pretty = 0)

# predicit

predictions <- predict(mytree_short,mydata_G)

table(predictions, mydata_G$Offer.Accepted)


# Decsion tree with rpart package
library(rpart)
tree1 <- rpart(NSPF ~ LB + AC + FM, trainD)


library(rpart.plot)
rpart.plot(tree1,extra = 2)

prp(tree1,extra = 2)

predict(tree1,validate)


#Misclassification Error


tab = table(predict(tree),trainD$NSPF)
print





















names(mydata)
names(lb.data)


# Doing the Random Split on mydata
library(caTools)
fil = sample.split(mydata$Offer.Accepted,SplitRatio = 0.75) 
rf.train = subset(mydata, fil == TRUE)
rf.test = subset(mydata,fil == FALSE)
names(rf.train)



# Random Forest Implementation
library(randomForest)
rf_TD <- randomForest(Offer.Accepted ~. -cm_key ,data = rf.train, importance = TRUE)

print(rf_TD)

print(rf)

rf_TD$importance
rf_

p <- predict(rf_TD,rf.test)

table(Predictions = p, Actual = rf.test$Offer.Accepted)

varImpPlot(rf_TD,sort = T,n.var = 10,
          main = 'HI I am here')

importance(rf) # prints out importance
varUsed(rf)

# Random Forest on Complete Training Data

data <- training_dataset

write.csv(data,'training_dataset_names.csv')


names(data)

CSP <- data.frame(table(data$cust.spend.cap))

library(dplyr)

head(arrange(CSP,desc(Freq)),10)

IN <- data.frame(table(data$income))
head(arrange(IN,desc(Freq)),10)


