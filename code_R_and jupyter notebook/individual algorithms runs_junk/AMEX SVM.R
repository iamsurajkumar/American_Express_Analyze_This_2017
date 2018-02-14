
# Gettign the Data

str(mydata)

# Excluding Some varibles from the model

mydata.sub = mydata[,!names(mydata) %in% vars.exclude]

str(mydata.sub)
names(mydata.sub)




# Doing the Random Split on mydata
library(caTools)
fil = sample.split(mydata.sub$IV_Accept,SplitRatio = 0.75) 
svm.train = subset(mydata.sub, fil == TRUE)
svm.test = subset(mydata.sub,fil == FALSE)
names(svm.train)
head(svm.train,10)



vars.exclude <- c('card.type',
           'IV_accept_Supp',
           'IV_accept_Elite',
           'IV_accept_Credit',
           'kCluster',
           'cust.spend.cap',
           'spend.indus.code',
           'Offer.Extended')







# SVM Implementation for IV_Accept
library(e1071)
svmfit <- svm(IV_Accept ~ . - cm_key,data = svm.train[1:50,],kernel = 'linear', cost = 10) 


names(svm.train)
any(is.na(svm.train))
is.na.data.frame(svm.test)
