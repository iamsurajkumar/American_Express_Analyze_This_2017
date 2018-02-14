# Training dataset

names(train)

# Importing the libraries for the KNN Model


library(class)
set.seed(1)

library(caTools)

mydata = mydata_G

names(mydata)

# Standarzing the Data
str(mydata)
standardized.mydata = scale(mydata[,-c(1,13)])
head(standardized.mydata)


# Doing the Train Test Split on Train Data

split <- sample.split(mydata$Offer.Accepted,SplitRatio = 0.75)
train.KNN <- subset(standardized.mydata, split == TRUE)
test.KNN<- subset(standardized.mydata, split == FALSE)


str(train.KNN)
str(test.KNN)
str(mydata)

knn.pred <- knn(train.KNN,test.KNN,mydata$Offer.Accepted[split],k=5)

table(knn.pred,mydata$Offer.Accepted[!split])

misClasificError <- mean(knn.pred != mydata$Offer.Accepted[!split])

misClasificError

# Choosing Optimal K-value

knn.predict <- NULL
error.rate <- NULL

for (i in 1:100){
  set.seed(1)
  knn.predict <- knn(train.KNN,test.KNN,train.KNN.global$Offer.Accepted,k=i)
  error.rate[i] <- mean(knn.predict != test.KNN.global$Offer.Accepted)
}

print(error.rate)


# Visualization

library(ggplot2)
k.values <- 1:100

error.df <- data.frame(error.rate,k.values)

error.df

ggplot(error.df,aes(x = k.values,y = error.rate))+ geom_point()+ geom_line(lty = 'dotted',color = 'red')


