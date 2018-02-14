
'
I am going to breakdown this problem into two steps,

First make customers into two categories offer and not offered. and
then select which offer to make

'

# Figuring Out Shall I offer or not

names(mydata)
names(lb.data)


# Doing the Random Split on mydata ####
library(caTools)
fil = sample.split(mydata$IV_Accept,SplitRatio = 0.75) 
rf.train = subset(mydata, fil == TRUE)
rf.test = subset(mydata,fil == FALSE)
names(rf.train)
head(rf.train,10)


#Formula Creation ####
a <-c('cm_key','card.type',
      'IV_extend_Supp',
      'IV_extend_Elite',
      'IV_extend_Credit',
      'IV_accept_Supp',
      'IV_accept_Elite',
      'IV_accept_Credit')

b <- paste('cm_key','card.type',
           'IV_extend_Supp',
           'IV_extend_Elite',
           'IV_extend_Credit',
           'IV_accept_Supp',
           'IV_accept_Elite',
           'IV_accept_Credit','kCluster',sep = ' - ' )

c <- paste('cm_key','card.type',
           'IV_extend_Supp',
           'IV_extend_Elite',
           'IV_extend_Credit',
           'IV_accept_Supp',
           'IV_accept_Elite',
           'IV_accept_Credit','kCluster','IV_Accept',sep = ' - ' )

vars.excluded <- paste('-',b)
vars.excluded

vars.excluded_2 <- paste('-',c)
vars.excluded_2


f <- paste('IV_Accept', "~", '.',vars.excluded)
af=as.formula(f)
af

#Different Formula
f1 <- paste('Offer.Extended', "~", '.',vars.excluded_2)
af1 = as.formula(f1)

af1
str(rf.train)
names(rf.train)

# Random Forest Implementation for IV_Accept ####
library(randomForest)
rf_TD <- randomForest(af,data = rf.train, importance = TRUE)
rf_TD_ex <- randomForest(af1,data = rf.train, importance = TRUE)
print(rf_TD_ex)

rf_TD_ex_1000 <- randomForest(af1,data = rf.train,ntree = 1000, importance = TRUE)

imp <-rf_TD_ex$importance

print(rf_TD_ex_1000)


# Tuning the RF####
tuneRF(Boston[train,-14],Boston[train,14],
       stepFactor = 0.5,
       plot = TRUE, ntreeTry = 500,
       trace = TRUE,
       improve = 0.05)

names(rf.train)


tuneRF(rf.train[,-c(47,48,49,50,51,52,53)],rf.train[,54],
       stepFactor = 0.1,
       plot = TRUE, ntreeTry = 500,
       trace = TRUE,
       improve = 0.05)

rf_TD_m1 <- randomForest(af,data = rf.train,mtry = 9, importance = TRUE)
rf_TD_m4 <- randomForest(af,data = rf.train,mtry = 4, importance = TRUE)
rf_TD_ex <- randomForest(af1,data = rf.train,mtry = 9, importance = TRUE)
print(rf_TD_m1)
print(rf_TD_m4)


# Testing the Model on Test Data ####
p <- predict(rf_TD,rf.test)
table(Predictions = p, Actual = rf.test$IV_Accept)
varImpPlot(rf_TD_ex,sort = T,n.var = 20,
           main = 'Variable Importance Factor')

importance(rf_TD) # prints out importance
varUsed(rf)







# Random Forest for Customer.Category ####

library(caTools)
fil = sample.split(dat_gi$Customer.Category,SplitRatio = 0.75) 
rf.train = subset(dat_gi, fil == TRUE)
rf.test = subset(dat_gi,fil == FALSE)
names(rf.train)
head(rf.train,10)

# Model creation

rf <- randomForest(Customer.Category ~. -cm_key, data = rf.train,importance = TRUE)

print(rf)

table(Predictions = predict(rf,rf.train), Actual = rf.train$Customer.Category)
table(Predictions = predict(rf,rf.test), Actual = rf.test$Customer.Category)
