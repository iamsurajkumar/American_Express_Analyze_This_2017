library(doParallel)
cl <- makeCluster(detectCores()) 
registerDoParallel(cl)



# Importing the Data

training_dataset <- read.csv('Training_Dataset.csv')
train <- read.csv('training_dataset_complete.csv')

data_names <- c('cm_key','card.type','fam.size'
                ,'cust.spend.cap','total.active.cards','account.age',
                'cm.fees','high.spend.afinity','int.inf.score',
                'income','plcard','int.busi.exp.score',
                'spend.indus.code','freq.payments','cm.number',
                'air.miles.mem',
                'elec_q1','elec_q2','elec_q3','elec_q4',
                'trav_q1','trav_q2','trav_q3','trav_q4',
                'hou_q1','hou_q2','hou_q3','hou_q4',
                'car_q1','car_q2','car_q3','car_q4',
                'retail_q1','retail_q2','retail_q3','retail_q4',
                'tot_q1','tot_q2','tot_q3','tot_q4',
                'freq_extend_Supp', 'freq_extend_Elite',
                'freq_extend_Credit',
                'freq_accept_Supp',
               'freq_accept_Elite',
                'freq_accept_Credit',
                'IV_extend_Supp',
                'IV_extend_Elite',
                'IV_extend_Credit',
                'IV_accept_Supp',
                'IV_accept_Elite',
                'IV_accept_Credit')

data_names

colnames(training_dataset) <- data_names

head(training_dataset)

train <- training_dataset

names(train)

# 

offername <- function(s,e,c){
  if(s == 1){
    return('Supp')
  } else if(e == 1){
    return('Elite')
  } else if(c == 1){
    return('Credit')
  } else{
    return('No Offer')
  }
}
offername(0,0,0)

head(train,30)



# Let Me Create Function for Customer Response ####




custResponse(0,0,1,0,0,0)

whataccepted <- function(f_a_s,f_a_e,f_a_c){
  if(f_a_s > 0){
    return('Supp')
  } else if(f_a_e > 0){
    return('Elite')
  } else{
    return('Credit')
  }
}

whatoffered <- function(f_e_s,f_e_e,f_e_c){
  if(f_e_s > 0){
    return('Supp')
  } else if(f_e_e > 0){
    return('Elite')
  } else{
    return('Credit')
  }
}

whataccepted(0,0,1)
whatoffered(0,0,1)

custResponse <- function(f_e_s,f_e_e,f_e_c,f_a_s,f_a_e,f_a_c){
  f_a = f_a_s + f_a_e + f_a_c
  if(f_a > 0){
    f_a_X = whataccepted(f_a_s,f_a_e,f_a_c)
    f_e_Y = whatoffered(f_e_s,f_e_e,f_e_c)
    if(f_a_X == f_e_Y){
      return(2)
    } else{
      return(1)
    }
  }else{
    return(0)
  }
}


train$Customer.Response <- mapply(custResponse, train$freq_extend_Supp,train$freq_extend_Elite,train$freq_extend_Credit,train$freq_accept_Supp,train$freq_accept_Elite,train$freq_accept_Credit)

table(train$Customer.Response)


#### For combined effect of quarter variables ####

train$elec <- train$elec_q1 + train$elec_q2+train$elec_q3 + train$elec_q4
train$trav <- train$trav_q1 + train$trav_q2 + train$trav_q3+train$trav_q4
train$hou <- train$hou_q1+ train$hou_q2+train$hou_q3+train$hou_q4
train$car <- train$car_q1 + train$car_q2+ train$car_q3+train$car_q4 
train$retail <- train$retail_q1 +train$retail_q2+train$retail_q3+ train$retail_q4
train$total.spend <- train$tot_q1 +train$tot_q2+train$tot_q3 + train$tot_q4
train$Offer.Accepted <- mapply(offername, train$IV_accept_Supp, train$IV_accept_Elite,train$IV_accept_Credit)

# Factor Converstion

str(train)

train$Offer.Accepted <- as.factor(train$Offer.Accepted)
train$Customer.Response <- as.numeric(train$Customer.Response)
train$plcard <- (training.dataset$plcard)

str(train)
names(train)

# Separating out the variables of interest #####

model.vars <- c('cm_key','fam.size','total.active.cards',
                'account.age',
                'cm.fees','high.spend.afinity','int.inf.score',
                'income','plcard','int.busi.exp.score',
                'freq.payments','cm.number','total.spend',
                'Customer.Response','Offer.Accepted')


min.model.vars <- c('cm_key','fam.size','total.active.cards','account.age',
                'cm.fees',
                'income','plcard',
                'freq.payments','total.spend',
                'Customer.Response','Offer.Accepted')

length(model.vars)
length(names(train))

mydata <- train[,names(train) %in% model.vars]
mydata_i1 <- train_i1[,names(train_i1) %in% model.vars]
mydata_i2 <- train_i2[,names(train_i2) %in% model.vars]

length(names(mydata))

library(corrplot)

subset(mydata,drop = c('cm_key','plcard','Customer.Response'))
corrplot(mydata[,-1], method = 'pie')
corrplot(M, method = 'number')



# Lets Create Two sets of data due to income anomoly #####
library(dplyr)
arrange(as.data.frame(table(training_dataset$income)),desc('Freq'))



f.i1 <- train$income != 2918974
f.i2 <- train$income != 0
sum(f.i1)
sum(f.i2)
f.i = f.i1 &f.i2
sum(f.i)

train_i1 <- train[ f.i,]
train_i2 <- train[!f.i1,]

dim(train_i1)
dim(train_i2)





# Figuring out the Missign values For Income and Customer Spending capacity ####
library(dplyr)
data$income[data$income == 0] = NA
data$cust.spend.cap[data$cust.spend.cap == 0] = NA

head(data,20)

library(DMwR)
data.Out <- knnImputation(data)  # perform knn imputation.
anyNA(data.Out)
anyNA(data)
# Missing Values

mydata <- data.Out
# Pattern of missing values
library(mice)
md.pattern(mydata)  # pattern or missing values in data.


str(mydata)
write.csv(mydata,'training_dataset_complete.csv')

# Using KMeans Clustering to Clusterdata into 2 categories ####

names(mydata)
km.out = kmeans(mydata[,-c(-1,-2,-13)],2,nstart = 50)
head(km.out,10)
table(km.out$cluster)

mydata[,c(-1,-2,-13)]
mydata$kCluster = km.out$cluster

library(ggplot2)
qplot()
cor(mydata[,-c(-1,-2,-13)])

mydata$IV_Accept = as.numeric(as.character(mydata$IV_accept_Credit)) + 
                    as.numeric(as.character(mydata$IV_accept_Elite)) + 
                    as.numeric(as.character(mydata$IV_accept_Supp))
mydata$Offer.Extended = as.factor(mapply(offername, mydata$IV_extend_Supp, mydata$IV_extend_Elite,mydata$IV_extend_Credit))

mydata$IV_Accept = as.factor(mydata$IV_Accept)
mydata$kCluster = as.factor(mydata$kCluster)
str(mydata)

table(data$IV_Accept)
table(data$cm.number)

8878/31122
8878/40000
head(mydata,10)



str(train)

any((train$IV_extend_Credit > 0) & (train$IV_accept_Elite > 0))


Customer.Category <- function(f_e_s,f_e_e,f_e_c,f_a_s,f_a_e,f_a_c){

  if(f_e_s > 0 ){
    
    if(f_a_s > 0){
      return('YS')
    } else{
      return('NS')
    }
    
  } else if(f_e_e > 0){
    
    if(f_a_e > 0){
      return('YE')
    } else {
      return('NE')
    }
    
  } else {
    
    if(f_a_c > 0){
      return('YC')
    } else {
      return('NC')
    }
  }
  
}

Customer.Category(1,0,0,1,0,0)
Customer.Category(0,0,1,0,0,1)

train$Customer.Category <- mapply(Customer.Category,train$IV_extend_Supp,train$IV_extend_Elite,
                                  train$IV_extend_Credit, train$IV_accept_Supp,
                                  train$IV_accept_Elite, train$IV_accept_Credit)

(table(train$Customer.Category))

train$Customer.Category <- as.factor(train$Customer.Category)

str(mydata)
str(train)

# Factor datasets ####

mydata <- train
mydata$plcard = as.factor(mydata$plcard)
mydata$IV_accept_Supp =as.factor(mydata$IV_accept_Supp)
mydata$IV_accept_Elite =as.factor(mydata$IV_accept_Elite)
mydata$IV_accept_Credit =as.factor(mydata$IV_accept_Credit)
mydata$Customer.Category = as.factor(mydata$Customer.Category)
str(mydata)


# Droping some IV Variables 

drop.vars <- c('IV_extend_Supp','IV_extend_Elite','IV_extend_Credit',
               'IV_accept_Supp','IV_accept_Elite','IV_accept_Credit',
               'card.type','cust.spend.cap','X')


dat <- mydata[,!names(mydata) %in% drop.vars]

str(dat)

# Sampling 1000 observations ####

index <- sample(40000,1000,replace = FALSE)



smalldat <-dat[index,]  # Small data created

table(index)

# Income Separation ####

dat_gi <- dat[dat$incom != 2918974,]
sum(dat$income != 2918974)

dim(dat_gi)

index2 <- sample(nrow(dat_gi),1000,replace =FALSE) 

smalldat_gi <- dat_gi[index2,]

dim(smalldat_gi)




### SEPARATE MODEL DATASETS #####

# Datasets for Supp Regression ####

drop.vars.Supp <- c('IV_extend_Supp','IV_extend_Elite','IV_extend_Credit'
               ,'IV_accept_Elite','IV_accept_Credit',
               'card.type','cust.spend.cap','X',
               'Customer.Category',
               'elec_q1','elec_q2','elec_q3','elec_q4',
               'trav_q1','trav_q2','trav_q3','trav_q4',
               'hou_q1','hou_q2','hou_q3','hou_q4',
               'car_q1','car_q2','car_q3','car_q4',
               'retail_q1','retail_q2','retail_q3','retail_q4',
               'tot_q1','tot_q2','tot_q3','tot_q4')


dat.Supp <- mydata[,!names(mydata) %in% drop.vars.Supp]

str(dat.Supp)


# Datasets for Elite Regression ####

drop.vars.Elite <- c('IV_extend_Supp','IV_extend_Elite','IV_extend_Credit'
                    ,'IV_accept_Supp','IV_accept_Credit',
                    'card.type','cust.spend.cap','X','Customer.Category')


dat.Elite <- mydata[,!names(mydata) %in% drop.vars.Elite]

str(dat.Elite)

table(dat.Elite$IV_accept_Elite)


# Datasets for Credit Regression

drop.vars.Credit <- c('IV_extend_Supp','IV_extend_Elite','IV_extend_Credit'
                     ,'IV_accept_Supp','IV_accept_Elite',
                     'card.type','cust.spend.cap','X','Customer.Category')


dat.Credit <- mydata[,!names(mydata) %in% drop.vars.Credit]

str(dat.Credit)

table(dat.Credit$IV_accept_Credit)

names(dat.Credit)
names(dat.Elite)
names(dat.Supp)


# DATASETS FOR SEPARATE 3 ####

trim.vars <- c('card.type','cust.spend.cap',
          'elec_q1','elec_q2','elec_q3','elec_q4',
          'trav_q1','trav_q2','trav_q3','trav_q4',
          'hou_q1','hou_q2','hou_q3','hou_q4',
          'car_q1','car_q2','car_q3','car_q4',
          'retail_q1','retail_q2','retail_q3','retail_q4',
          'tot_q1','tot_q2','tot_q3','tot_q4',
          'IV_extend_Supp','IV_extend_Elite','IV_extend_Credit'
          ,'IV_accept_Supp','IV_accept_Elite','IV_accept_Credit',
          'Customer.Category','X')



mydata.trim <- mydata[,!names(mydata) %in% trim.vars]
length(names(mydata))
length((trim.vars))
length(mydata.trim)
str(mydata.trim)
head(mydata)


# Creating a dataset of where IV_Accept == 1

fil = mydata$IV_Accept == 1
fil

mydata.trim.accept = mydata[fil,]
mydata.trim.accept$Offer.Accepted = as.factor(mapply(offer_accept,
                            mydata.trim.accept$IV_accept_Supp,
                            mydata.trim.accept$IV_accept_Elite,
                            mydata.trim.accept$IV_accept_Credit))

trim2.vars <- c('card.type','cust.spend.cap',
                  'elec_q1','elec_q2','elec_q3','elec_q4',
                  'trav_q1','trav_q2','trav_q3','trav_q4',
                  'hou_q1','hou_q2','hou_q3','hou_q4',
                  'car_q1','car_q2','car_q3','car_q4',
                  'retail_q1','retail_q2','retail_q3','retail_q4',
                  'tot_q1','tot_q2','tot_q3','tot_q4',
                  'IV_extend_Supp','IV_extend_Elite','IV_extend_Credit'
                  ,
                  'X','Customer.Category','IV_Accept')

mydata.trim.accept = mydata.trim.accept[,!names(mydata.trim.accept) %in% trim2.vars]
str(mydata.trim.accept)
names(mydata.trim.accept)

#Creating a Dataframe for rejected entries
mydata.trim.reject = mydata[!fil,]
mydata.trim.reject$Offer.Accepted = as.factor(mapply(offer_accept,
                                                     mydata.trim.reject$IV_accept_Supp,
                                                     mydata.trim.reject$IV_accept_Elite,
                                                     mydata.trim.reject$IV_accept_Credit))

mydata.trim.reject = mydata.trim.reject[sample(nrow(mydata.trim.reject), 9000, replace = F),
                                        !names(mydata.trim.reject) %in% trim2.vars]

head(mydata.trim.reject)
str(mydata.trim.reject)
names(mydata.trim.reject)


head(mydata.trim.reject)
table(mydata.trim.reject$Offer.Accepted)


mydata.trim.comb <- rbind(mydata.trim.accept,mydata.trim.reject)

head(mydata.trim.comb)
str(mydata.trim.comb)
names(mydata.trim.comb)
table(mydata.trim.comb$Offer.Accepted)


offer_accept <- function(s,e,c){
  if(s == 1){
    return('Supp')
  } else if(e == 1){
    return('Elite')
  } else if(c == 1){
    return('Credit')
  } else{
    return('No Offer')
  }
}

head(mydata.trim.accept)

#AMEX Leaderboard ####

lb <- read.csv('Leaderboard_Dataset.csv')

lb_names <- c('cm_key','card.type','fam.size'
              ,'cust.spend.cap','total.active.cards','account.age',
              'cm.fees','high.spend.afinity','int.inf.score',
              'income','plcard','int.busi.exp.score',
              'spend.indus.code','freq.payments','cm.number',
              'air.miles.mem',
              'elec_q1','elec_q2','elec_q3','elec_q4',
              'trav_q1','trav_q2','trav_q3','trav_q4',
              'hou_q1','hou_q2','hou_q3','hou_q4',
              'car_q1','car_q2','car_q3','car_q4',
              'retail_q1','retail_q2','retail_q3','retail_q4',
              'tot_q1','tot_q2','tot_q3','tot_q4',
              'freq_extend_Supp', 'freq_extend_Elite',
              'freq_extend_Credit',
              'freq_accept_Supp',
              'freq_accept_Elite',
              'freq_accept_Credit')


length(names(lb))
length(lb_names)

colnames(lb) <- lb_names

names(lb)
str(lb)

# Factor Conversion

lb$plcard <- as.factor(lb$plcard)

# Checking the lb for missing values

sum(lb$income == 0)
sum(lb$cust.spend.cap == 0)
lb$income[lb$income == 0] = NA
lb$cust.spend.cap[lb$cust.spend.cap == 0] = NA

#Imputing the Missing values for lb for Income and Cust.Spend.Cap
library(DMwR)
mylb <- knnImputation(lb)  # perform knn imputation.
anyNA(mylb)
anyNA(lb)





# Combining the Quarter Variables

lb$elec <- lb$elec_q1 + lb$elec_q2+lb$elec_q3 + lb$elec_q4
lb$trav <- lb$trav_q1 + lb$trav_q2 + lb$trav_q3+lb$trav_q4
lb$hou <- lb$hou_q1+ lb$hou_q2+lb$hou_q3+lb$hou_q4
lb$car <- lb$car_q1 + lb$car_q2+ lb$car_q3+lb$car_q4 
lb$retail <- lb$retail_q1 +lb$retail_q2+lb$retail_q3+ lb$retail_q4
lb$total.spend <- lb$tot_q1 +lb$tot_q2+lb$tot_q3+lb$tot_q4
lb$Customer.Response <-  mapply(custResponse, lb$freq_extend_Supp,lb$freq_extend_Elite,lb$freq_extend_Credit,lb$freq_accept_Supp,lb$freq_accept_Elite,lb$freq_accept_Credit)


# Building building the train.KNN

names(lb)

lb.data <- lb[,names(lb) %in% model.vars]
length(names(lb.data))


#Comparing mydata nd lb.data

names(mydata)
names(lb.data)


head(arrange(CSP,desc(Freq)),10)

IN <- data.frame(table(data$income))
head(arrange(IN,desc(Freq)),10)


# Preparing for lb for separate logistic regression

lb.vars <- c('card.type','cust.spend.cap')

lb.mydata <- mylb[,!names(mylb) %in% lb.vars]

str(lb.mydata)
length(names(lb.mydata))
length(names(dat.Supp))
length(names(dat.Elite))
length(names(dat.Credit))

names(lb.mydata == dat.Supp)

str(dat.Supp)
str(lb.mydata)
