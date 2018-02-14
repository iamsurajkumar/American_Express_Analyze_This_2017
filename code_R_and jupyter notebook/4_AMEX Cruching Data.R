length(names(training_dataset))

fill = training_dataset$freq_accept_Supp > training_dataset$freq_extend_Supp

training_dataset[fill,]
# So there are certain observation where more times Supp has been accepted than it has been extended, it doesn't make sense



fill2 = training_dataset$freq_accept_Elite > training_dataset$freq_extend_Elite

sum(fill2) # So elite is okay

fill3 = training_dataset$freq_accept_Credit > training_dataset$freq_extend_Credit

sum(fill3) # So credit is also okay

"
So one thing is for sure that if dependent varialbe exist then its 
going to be using something from the IVs

"
cor(training_dataset$freq_accept_Credit,training_dataset$IV_accept_Credit)


library(ggplot2)

ggplot(training_dataset,aes(x = freq_accept_Credit,y= IV_accept_Credit))+ geom_point()


sum(training_dataset$freq_accept_Credit >=0)
# so it shows that very less people really accepted the credit card, let me
# compare it with the

sum(training_dataset$freq_extend_Credit > 0)
#

sum(training_dataset$freq_extend_Supp > 0) # 9004 people were extended Supp
sum(training_dataset$freq_accept_Supp > 0)


sum(training_dataset$freq_extend_Elite > 0) # So 2318 people were given this card
sum(training_dataset$freq_accept_Elite > 0) # 


# Lets try to see some more correlation matrix beteen the variables

cor.vars <- c('freq_extend_Credit','freq_extend_Supp','freq_extend_Elite','freq_accept_Credit','freq_accept_Supp','freq_accept_Elite','IV_extend_Credit','IV_extend_Supp','IV_extend_Elite','IV_accept_Credit','IV_accept_Supp','IV_accept_Elite')


cor.mat <- training_dataset[,(names(training_dataset) %in% cor.vars)]

names(cor.mat)





M <- cor(cor.mat)

# Inference

library(corrplot)
corrplot(M, method = 'pie')
corrplot(M, method = 'number')


"
Elite offers are more likely to be accepted when extended then Supp and the last credit
So try to extend more elite, then Supp and then credit
"

# Questions to ask if one of IV_Accept is 1 then does it mean that others are necessarily zeros

sum(training_dataset$IV_accept_Supp == 0 & training_dataset$IV_accept_Elite == 0 & training_dataset$IV_accept_Credit == 0)



# Same above question for IV_extend

sum(training_dataset$IV_extend_Supp > 0) + sum(training_dataset$IV_extend_Elite > 0) + sum(training_dataset$IV_extend_Credit > 0)

sum(training_dataset$IV_accept_Supp > 0) + sum(training_dataset$IV_accept_Elite > 0) + sum(training_dataset$IV_accept_Credit > 0)



# So that means everyone in this list of 40,000 has been extend offer but only
# of them have accepted this offer. My task probably is

'
construct this IV.extend.OFFERNAME from my data

And to help this construction I have IV of acceptance to just 
check my construction is right or not.



'


# Answering a question is mean income of people who have accepted elite card is higher
# than income of peole who have accepted credit card
'
Average Income of card who accepted Supp is 2767830
Average Income of card who accepted Elite is 1761163
Average Income of card who accepted Supp is 1415448
Average Income of card who not accepted any offer is 1985587

'




f.e <- training_dataset$IV_accept_Elite > 0
f.s <- training_dataset$IV_accept_Supp > 0
f.c <- training_dataset$IV_accept_Credit > 0
f.i <- training_dataset$income != 0

mean(training_dataset$income[f.e & f.i])
mean(training_dataset$income[f.s &f.i])
mean(training_dataset$income[f.c & f.i])

mean(training_dataset$income[!f.e&!f.s&!f.c&f.i])

# Okay Let me think 


# SO Finally After this analysis my dependent variable is IV_accept  = IV_accept_Elite + IV_accept_Supp+IV_accept_Credit
# I am just dropping IV_extend variables as they are 


train <- training_dataset

train$IV_accept <- train$IV_accept_Credit +train$IV_accept_Elite+train$IV_accept_Supp


### Now checking is income explaining IV_accept
gen.fit <- glm(formula = IV_accept ~ income, family = binomial(link = 'logit'),data = train[f.i,])
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ income, family = binomial(link = 'logit'),data = train[f.i,])
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ income, family = binomial(link = 'logit'),data = train[f.i,])
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ income, family = binomial(link = 'logit'),data = train[f.i,])
summary(Credit.fit)

library(ggplot2)
ggplot(data = train,aes(x = IV_accept_Supp,y = log(income))) + geom_point()


#### fam.size explains IV_accept
gen.fit <- glm(formula = IV_accept ~ fam.size, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ fam.size, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ fam.size, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ fam.size, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### cust.spend.cap

# is customer spending capacity greater than her income

train[train$cust.spend.cap > train$income,] # so it could be

# is customer spending capacity correlated with income

cor(train$cust.spend.cap[!fil1 & !fil2],train$income[!fil1 & !fil2])
# answer is not much

fil1 <- train$cust.spend.cap == 0
fil2 <- train$income == 0
ggplot(data = train[!fil1 & !fil2,],aes(x = log(income),y = log(cust.spend.cap))) + geom_point()

# does it explains the IV_accept, well it should
gen.fit <- glm(formula = IV_accept ~ cust.spend.cap, family = binomial(link = 'logit'),data = train[!fil1,])
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ cust.spend.cap, family = binomial(link = 'logit'),data = train[!fil1,])
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ cust.spend.cap, family = binomial(link = 'logit'),data = train[!fil1,])
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ cust.spend.cap, family = binomial(link = 'logit'),data = train[!fil1,])
summary(Credit.fit)



#### Taking the Effect of total.active.cards
gen.fit <- glm(formula = IV_accept ~ total.active.cards, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ total.active.cards, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ total.active.cards, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ total.active.cards, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### Effect of Account.age
gen.fit <- glm(formula = IV_accept ~ account.age, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ account.age, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ account.age, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ account.age, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### Effect of cm.fees

gen.fit <- glm(formula = IV_accept ~ cm.fees, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ cm.fees, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ cm.fees, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ cm.fees, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### Effect of high.spend.affinity

gen.fit <- glm(formula = IV_accept ~ high.spend.affinity, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ high.spend.affinity, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ high.spend.affinity, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ high.spend.affinity, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)


#### Effect of int.inf.score

gen.fit <- glm(formula = IV_accept ~ int.inf.score, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ int.inf.score, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ int.inf.score, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ int.inf.score, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)


#### Effect of Plcard

gen.fit <- glm(formula = IV_accept ~ plcard, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ plcard, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ plcard, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ plcard, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### Effect of Int.Busi.exp.score

gen.fit <- glm(formula = IV_accept ~ int.busi.exp.score, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ int.busi.exp.score, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ int.busi.exp.score, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ int.busi.exp.score, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### Spend Industry Code ##
gen.fit <- glm(formula = IV_accept ~ spend.indus.code, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ spend.indus.code, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ spend.indus.code, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ spend.indus.code, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### Freq.payements
gen.fit <- glm(formula = IV_accept ~ freq.payments, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ freq.payments, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ freq.payments, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ freq.payments, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

#### For cm.number
gen.fit <- glm(formula = IV_accept ~ cm.number, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ cm.number, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ cm.number, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ cm.number, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)



#### For air.miles.mem
gen.fit <- glm(formula = IV_accept ~ air.miles.mem, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ air.miles.mem, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ air.miles.mem, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ air.miles.mem, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)




train$tot_q1 == train$elec_q1+train$trav_q1+train$hou_q1+train$car_q1+train$retail_q1

#### For combined effect of quarter variables

train$elec <- train$elec_q1 + train$elec_q2+train$elec_q3 + train$elec_q4
train$trav <- train$trav_q1 + train$trav_q2 + train$trav_q3+train$trav_q4
train$hou <- train$hou_q1+ train$hou_q2+train$hou_q3+train$hou_q4
train$car <- train$car_q1 + train$car_q2+ train$car_q3+train$car_q4 
train$retail <- train$retail_q1 +train$retail_q2+train$retail_q3+ train$retail_q4


## Ggplot

ggplot(train,aes(x = log(elec)))+ geom_histogram(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5)
ggplot(train,aes(x = log(trav)))+ geom_histogram(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5)
ggplot(train,aes(x = log(hou)))+ geom_histogram(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5)
ggplot(train,aes(x = log(car)))+ geom_histogram(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5)
ggplot(train,aes(x = log(retail)))+ geom_histogram(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5)



#### For elec

gen.fit <- glm(formula = IV_accept ~ elec, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ elec, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ elec, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ elec, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)


## For trav #####


## For hou #####

## For car ####

gen.fit <- glm(formula = IV_accept ~ car, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ car, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ car, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ car, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

## For Retail ####

## For tot #####

# for tot_q1

gen.fit <- glm(formula = IV_accept ~ tot_q4, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ tot_q4, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ tot_q4, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ tot_q4, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)







#### looking at the effects of freq variables

## freq_extend_Supp ####
gen.fit <- glm(formula = IV_accept ~ freq_extend_Supp, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ freq_extend_Supp, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ freq_extend_Supp, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ freq_extend_Supp, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

## freq_extend_Elite ####

gen.fit <- glm(formula = IV_accept ~ freq_extend_Elite, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ freq_extend_Elite, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ freq_extend_Elite, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ freq_extend_Elite, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)


## freq_extend_Credit

gen.fit <- glm(formula = IV_accept ~ freq_extend_Credit, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ freq_extend_Credit, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ freq_extend_Credit, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ freq_extend_Credit, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)

## freq_accept_Supp

gen.fit <- glm(formula = IV_accept ~ freq_accept_Supp, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ freq_accept_Supp, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ freq_accept_Supp, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ freq_accept_Supp, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)


## freq_accept_Elite

gen.fit <- glm(formula = IV_accept ~ freq_accept_Elite, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ freq_accept_Elite, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ freq_accept_Elite, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ freq_accept_Elite, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)


## freq_accept_Credit

gen.fit <- glm(formula = IV_accept ~ freq_accept_Credit, family = binomial(link = 'logit'),data = train)
summary(gen.fit)

Supp.fit <- glm(formula = IV_accept_Supp ~ freq_accept_Credit, family = binomial(link = 'logit'),data = train)
summary(Supp.fit)

Elite.fit <- glm(formula = IV_accept_Elite ~ freq_accept_Credit, family = binomial(link = 'logit'),data = train)
summary(Elite.fit)

Credit.fit <- glm(formula = IV_accept_Credit ~ freq_accept_Credit, family = binomial(link = 'logit'),data = train)
summary(Credit.fit)



### Now buliding a complete correlation matrix


drop.vars <- c('cm_key','card.type','spend.indus.code')


cor.mat <- train[,!(names(train) %in% drop.vars)]

names(cor.mat)





M <- cor(cor.mat)

# Inference

library(corrplot)
corrplot(M, method = 'pie')
corrplot(M, method = 'number')



# Train Include

