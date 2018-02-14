
# Getting the Data
mydata = mydata_G

str(mydata)
names(mydata)

# Doing the Train Test Split

library(caTools)


# Doing the Train Test Split on Train Data
set.seed(1)

split <- sample.split(mydata$Offer.Accepted,SplitRatio = 0.75)
train.kmeans <- subset(mydata, split == TRUE)
test.kmeans <- subset(mydata, split == FALSE)


str(train.kmeans)
str(kmean)


kmeansdata <- mydata[,-c(1,9,10)]

km.out = kmeans(kmeansdata,4,nstart = 50)
mydata$KCluster <- km.out$cluster

km.out$totss

with(mydata,table(KCluster,Offer.Accepted))

# 1 stands for No Offer
# 2 stands for Supp
# 3 stands Credit
# 4 for Elite


head(mydata,)






