# Coding Steps

# Data Buidling ####
library(caTools)
set.seed(123)
dat = mydata.trim[,-c(21,22,23)]

fil <- sample.split(dat$IV_Accept, SplitRatio = 0.75)
train1 <- subset(dat, fil == T)
test1 <- subset(dat,fil == F)

str(train1)
str(test1)
names(test1)
names(dat)

table(dat$IV_Accept)
length(lb.mydata)

#### CUSTOMER TARGETING ####

#mydata.trim.train <- mydata.trim.test

#### Using LOGISTIC REGRESSION ####

    Accept.log <- glm(IV_Accept ~. -cm_key,  
                      data = dat, 
                      family = binomial(link = 'logit') )
    
    summary(Accept.log)
    
    # Predicting Class Probability
    Accept.log.prob <- predict(Accept.log,lb.mydata,type = 'response')
    summary(Accept.log.prob)  
    head(Accept.log.prob)


#### Using RANDOM FOREST ####
    library(randomForest)
    Accept.rf <- randomForest(IV_Accept ~. -cm_key, 
                              data = dat,ntree = 500,
                              importance = TRUE)
    # Looking at the Model
    
    print(Accept.rf)
    Accept.rf$importance
    
    # Predicing Classes
    Accept.rf.P <- predict(Accept.rf,dat)
    table(Accept.rf.P, dat$IV_Accept)
    
    # Predicting Probability
    Accept.rf.Prob.Mat <- predict(Accept.rf,lb.mydata,type = 'prob')
    Accept.rf.Prob <- Accept.rf.Prob.Mat[,2]
    head(Accept.rf.Prob)
    head(Accept.rf.Prob.Mat)

# Using SUPPORT VECTOR MACHINES ####
   

    library(e1071)
    Accept.svm = svm(IV_Accept ~. -cm_key , 
                     data = dat , 
                     kernel = 'radial', 
                     cost = 1e5,gamma = 1,
                     decision.values =T, probability =T,
                     scale =F)
    
    summary(Accept.svm)
    print(Accept.svm)
    names(mydata.trim.train)
    
    
    # Transforming lb.mydata to be applied to Accept.svm
    names(lb.mydata.svm)
    lb.mydata.svm <- lb.mydata
    lb.mydata.svm$IV_accept_Supp = rep(0,nrow(lb.mydata.svm))
    lb.mydata.svm$IV_accept_Elite = rep(0,nrow(lb.mydata.svm))
    lb.mydata.svm$IV_accept_Credit = rep(0,nrow(lb.mydata.svm))
    
    # Calculation Matrix for Training Data
    A <- predict(Accept.svm,dat)
    table(predict = A, Actual = dat$IV_Accept)
    B <- predict(Accept.svm,mydata.trim.test)
    table(predict = B, Actual = mydata.trim.test$IV_Accept)
    
    
    # Probabilities Calculation for Test Data
    Accept.svm.Obj <- predict(Accept.svm, newdata = lb.mydata.svm,probability=TRUE)
    Accept.svm.Mat = data.frame(attr(Accept.svm.Obj, "probabilities"))
    Accept.svm.Prob = Accept.svm.Mat$X1
    str(Accept.svm.Prob)



# Now Aggregating These Models ####
    total.prob <- Accept.log.prob + Accept.rf.Prob + Accept.svm.Prob


#Now Adding this Probability Sum to train data
    library(dplyr)
    final2 <- lb.mydata
    final2$IV_Accept_Probability <- total.prob
    final2 <- arrange(final2, desc(IV_Accept_Probability))
    head(final2)
    names(final2)
    output <- final2[,-27]
    head(output)
    
    tempC <- ifelse(final2$Probability > 0.7,1,0)
    tempM <- cbind(final2$IV_Accept,final2$Probability)
    table(Predicted = tempC, Actual = final2$IV_Accept)


#### CLASSIFIATION MODEL ####
    
    mydata.trim.accept
    str(mydata.trim.accept)
    data = mydata.trim.comb

#Segregating the Test and Train Data

library(caTools)
set.seed(123)
fil3 <- sample.split(data$Offer.Accepted, SplitRatio = 0.75)
train <- subset(data, fil3 == T)
test <- subset(data,fil3 == F)
str(train)
str(test)

table(train$Offer.Accepted)


#LOGISTIC REGRESSION MODELS ####

  ## SUPP MODEL ##
  
      Cat.log.Supp <- glm(IV_accept_Supp ~.  -cm_key,
                          data = data[,-c(22,23,30)],
                          family = binomial(link = 'logit') )
      
      summary(Cat.log.Supp)
      
      # Probability Calculation For Train Data
      B <- predict(Cat.log.Supp, newdata = data[,-c(22,23,30)],
                   type = 'response')
      
      # Probability Calculation for Test Data
      Cat.log.Prob.Supp <- predict(Cat.log.Supp, 
                                   newdata = output,
                                  type = 'response')
      
  
  ## ELITE MODEL ##
  
      Cat.log.Elite <- glm(IV_accept_Elite ~.  -cm_key,
                          data = data[,-c(21,23,30)],
                          family = binomial(link = 'logit') )
      
      summary(Cat.log.Elite)
      
      # Probability Calculation For Train Data
      B <- predict(Cat.log.Elite, newdata = data[,-c(21,23,30)],
                   type = 'response')
      
      # Probability Calculation for Test Data
      Cat.log.Prob.Elite <- predict(Cat.log.Elite, 
                                   newdata = output,
                                   type = 'response')
      
  
  
  ## CREDIT MODEL ##
  
      Cat.log.Credit <- glm(IV_accept_Credit ~.  -cm_key,
                           data = data[,-c(21,22,30)],
                           family = binomial(link = 'logit') )
      
      summary(Cat.log.Credit)
      
      # Probability Calculation For Train Data
      B <- predict(Cat.log.Credit, newdata = data[,-c(21,22,30)],
                   type = 'response')
      
      # Probability Calculation for Test Data
      Cat.log.Prob.Credit <- predict(Cat.log.Credit, 
                                    newdata = output,
                                    type = 'response')
    



## DECISION TREE MODELS ####
library(randomForest)

  ## SUPP MODEL ##
      
      Cat.rf.Supp <- randomForest(IV_accept_Supp ~. -cm_key  , 
                             data = data[,-c(22,23,30)],
                             importance = TRUE)
      print(Cat.rf.Supp)
      
      
      # Confusion Matrix for Train Data
      
      Supp.rf.P <- predict(Cat.rf.Supp,data[,-c(22,23,30)])
      table(Supp.rf.P,data$IV_accept_Supp)
      
      # Probability Calculation for Test Data
      Supp.rf.Prob.Mat <- predict(Cat.rf.Supp,output,
                                  type = 'prob')
      Cat.rf.Prob.Supp <- Supp.rf.Prob.Mat[,2]
      
      head(Cat.rf.Prob.Supp)
      head(Supp.rf.Prob.Mat)
  
  ## ELITE MODEL ##
      
      Cat.rf.Elite <- randomForest(IV_accept_Elite ~. -cm_key  , 
                                  data = data[,-c(21,23,30)],
                                  importance = TRUE)
      print(Cat.rf.Elite)
      
      
      # Confusion Matrix for Train Data
      
      Elite.rf.P <- predict(Cat.rf.Elite,data[,-c(21,23,30)])
      table(Elite.rf.P,data$IV_accept_Elite)
      
      # Probability Calculation for Test Data
      Elite.rf.Prob.Mat <- predict(Cat.rf.Elite,output,
                                   type = 'prob')
      Cat.rf.Prob.Elite <- Elite.rf.Prob.Mat[,2]
      
      head(Cat.rf.Prob.Elite)
      head(Elite.rf.Prob.Mat)  
   
  ## CREDIT MODEL ##
      
      Cat.rf.Credit <- randomForest(IV_accept_Credit ~. -cm_key  , 
                                   data = data[,-c(21,22,30)],
                                   importance = TRUE)
      print(Cat.rf.Credit)
      
      
      # Confusion Matrix for Train Data
      Credit.rf.P <- predict(Cat.rf.Credit,data[,-c(21,22,30)])
      table(Credit.rf.P,data$IV_accept_Credit)
      
      # Probability Calculation for Test Data
      Credit.rf.Prob.Mat <- predict(Cat.rf.Credit,output,
                                   type = 'prob')
      Cat.rf.Prob.Credit <- Credit.rf.Prob.Mat[,2]
      
      head(Cat.rf.Prob.Credit)
      head(Credit.rf.Prob.Mat)   
  
names(data)
str(data)

## SVM ####
    library(e1071)
    Cat.svm = svm(Offer.Accepted ~.  -cm_key, 
                  data = data[,-c(21,22,23)], 
                  kernel = 'radial', 
                  cost = 1e3,gamma = 1,
                  decision.values =T,
                  scale = F,
                  probability=TRUE)
    
    summary(Cat.svm)
    
    #Confusion Matrix
    A <- predict(Cat.svm,test)
    table(predict = A, Actual = test$Offer.Accepted)
    
    #Probability Matrix
    Cat.svm.Obj <- predict(Cat.svm, newdata = output,probability=TRUE)
    Cat.svm.Mat = data.frame(attr(Cat.svm.Obj, "probabilities"))
    Cat.svm.Prob.Credit <- Cat.svm.Mat$Credit
    Cat.svm.Prob.Supp <- Cat.svm.Mat$Supp
    Cat.svm.Prob.Elite <- Cat.svm.Mat$Elite



## COMBINING THESE 3 MODELS TO PREDICT CORRECT CATEGORY ####

# Probability Sum
Prob.Credit <-  Cat.rf.Prob.Credit+ Cat.log.Prob.Credit + Cat.svm.Prob.Credit
Prob.Elite <- + Cat.rf.Prob.Elite + Cat.log.Prob.Elite  + Cat.svm.Prob.Elite
Prob.Supp <-  + Cat.rf.Prob.Supp + Cat.log.Prob.Supp +Cat.svm.Prob.Supp

#Category Prediction
Category.Predicted <- mapply(Category.Prob,
                             Prob.Supp,
                             Prob.Elite,
                             Prob.Credit)

head(Category.Predicted) 

#Adding Prob.XXXX and Category.Predicted Bact to Train

    final <- output
    final$Prob.Credit <- Prob.Credit
    final$Prob.Elite <- Prob.Elite
    final$Prob.Supp <- Prob.Supp
    final$Category.Predicted <- Category.Predicted
    final$Prob.Summ <- Prob.Credit + Prob.Elite + Prob.Supp
    str(final)  
    head(final)

  #Steps only for Output
    names(final2)
    final$IV_Prob <-final2[,27]
    cor(final$Prob.Summ,final$IV_Prob)
    
  # Confusion Matrix for Final
    CM <- table(Predicted = final$Category.Predicted,Actual = final$Offer.Accepted)
    CM
    diag(CM)/sum(CM)

    
    
    
  # Ordering Final
    final$Order.Prob = final$Prob.Summ+final$IV_Prob
    final.order <- head(arrange(final,desc(Order.Prob)),1000)
    head(final.order)
    str(final.order)
    
#Wrting The Results
head(final)
LFS3.results <- data.frame(cm_key = final.order$cm_key,
                          Category = final.order$Category.Predicted)

head(LFS1.results,20)

table(Simple = LFS.results$Category,Complex = LFS1.results$Category)

write.csv(LFS3.results,'Closed&Bounded_DSE_33.csv',row.names = F,col.names = F)

# Results by Ordering

library(dplyr)

# create a vector with letters in the desired order
x <- c("Elite", "Supp", "Credit")

LFS.results %>% slice(match(x, Category))

rf.results.order <- arrange(LFS.results,-desc(Category))
head(rf.results.order)
write.csv(rf.results.order,'Closed&Bounded_DSE_32.csv',row.names = F,col.names = F)





# Leaderboard Data Code

names(lb.mydata)
names(mydata.trim.train)
