
library(ggplot2)


names(training_dataset)

###### Histograms for Variables

# For Fam Size
ggplot(training_dataset,aes(x = fam.size))+ geom_histogram(binwidth = 1)

# For Card Type
ggplot(training_dataset,aes(x = card.type ))+ geom_bar()

# For Cust.Spend.cap

'Lets us first remove the 0 values and see the data'

fil <- training_dataset$cust.spend.cap == 0

sum(fil)
head(training_dataset[fil,],10)

ggplot(training_dataset[!fil,],aes(x = log(cust.spend.cap) ))+ geom_histogram(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5)

# For total.active.cards

ggplot(training_dataset,aes(x = total.active.cards ))+ geom_bar(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5)

table(training_dataset$total.active.cards)


# For Account.age

ggplot(training_dataset,aes(x = account.age ))+ geom_histogram(binwidth = 12,color = 'red',fill ='pink',alpha = 0.5)
table(training_dataset$account.age)

# So its right skwed

# For CM Fees

ggplot(training_dataset,aes(x = cm.fees ))+ geom_histogram(binwidth = 100,color = 'red',fill ='pink',alpha = 0.5) 


# For high.spend.afinity Score


ggplot(training_dataset,aes(x = high.spend.affinity ))+ geom_histogram(binwidth = 0.5,color = 'red',fill ='pink',alpha = 0.5) 

sum(training_dataset$high.spend.affinity > 0)


# For Interval Influencer Score

ggplot(training_dataset,aes(x = int.inf.score ))+ geom_histogram(binwidth = 0.1,color = 'red',fill ='pink',alpha = 0.5) 
sum(training_dataset$int.inf.score > 1)

# For Income

'let remove zero values  of income'
fil <- training_dataset$income == 0
ggplot(training_dataset[!fil,],aes(x = log(income) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

arrange(as.data.frame(table(training_dataset$income)),desc(Freq))

# so income seems to have a very weird distribution 

# so lets just remove this weird observation 2918974 from my data

fil <- (training_dataset$income != 2918974) &(training_dataset$income != 0)

ggplot(training_dataset[fil,],aes(x = log(income) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

# For plcard

table(training_dataset$plcard)

# For int.busi.exp.score

ggplot(training_dataset,aes(x = int.busi.exp.score ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

summary(training_dataset$int.busi.exp.score)

# For spend.indus.code

ggplot(training_dataset,aes(x = spend.indus.code ))+ geom_bar(color = 'red',fill ='pink',alpha = 0.5) 

arrange(as.data.frame(table(training_dataset$spend.indus.code)),desc(Freq))


mean(as.character(training_dataset$spend.indus.code) == 'Produce')

sum(training_dataset$spend.indus.code == 'Unknown')

str(training_dataset)

# for Freq Payments

ggplot(training_dataset,aes(x = freq.payments ))+ geom_histogram(binwidth = 1,color = 'red',fill ='pink',alpha = 0.5) 

table(training_dataset$freq.payments)


# for cm.number

as.data.frame(table(training_dataset$cm.number))

# for air.miles.mem

table(training_dataset$air.miles.mem)

# For electornic quarter data

#q1

ggplot(training_dataset,aes(x = log(elec_q1) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q2
ggplot(training_dataset,aes(x = log(elec_q2) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q3
ggplot(training_dataset,aes(x = log(elec_q3) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q4
ggplot(training_dataset,aes(x = log(elec_q4) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 


# For travel data

#q1

ggplot(training_dataset,aes(x = log(trav_q1) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q2

ggplot(training_dataset,aes(x = log(trav_q2) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q3

ggplot(training_dataset,aes(x = log(trav_q3) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q4

ggplot(training_dataset,aes(x = log(trav_q4) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 


# For House Data

#q1

ggplot(training_dataset,aes(x = log(hou_q1) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q2

ggplot(training_dataset,aes(x = log(hou_q2) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q3

ggplot(training_dataset,aes(x = log(hou_q3) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 


#q4

ggplot(training_dataset,aes(x = log(hou_q4) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

# For Car Data

#q1

ggplot(training_dataset,aes(x = log(car_q1) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q2

ggplot(training_dataset,aes(x = log(car_q2) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 
#q3

ggplot(training_dataset,aes(x = log(car_q3) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 
#q4

ggplot(training_dataset,aes(x = log(car_q4) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 


# For Retail Data
#q1

ggplot(training_dataset,aes(x = log(retail_q1) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q2

ggplot(training_dataset,aes(x = log(retail_q2) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 
#q3

ggplot(training_dataset,aes(x = log(retail_q3) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 
#q4

ggplot(training_dataset,aes(x = log(retail_q4) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

# For Total Data

#q1

ggplot(training_dataset,aes(x = log(tot_q1) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 

#q2

ggplot(training_dataset,aes(x = log(tot_q2) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 
#q3

ggplot(training_dataset,aes(x = log(tot_q3) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 
#q4

ggplot(training_dataset,aes(x = log(tot_q4) ))+ geom_histogram(bins = 100,color = 'red',fill ='pink',alpha = 0.5) 


# For freq_extend_Supp

summary(training_dataset$freq_extend_Supp)
table(training_dataset$freq_extend_Supp)

# For freq_extend_Elite

table(training_dataset$freq_extend_Elite)

# for freq_extend_Credit

table(training_dataset$freq_extend_Credit)

cor(cbind(training_dataset$freq_extend_Supp,training_dataset$freq_extend_Credit,training_dataset$freq_extend_Elite))

fil <- (training_dataset$freq_extend_Supp > 0)&(training_dataset$freq_extend_Credit > 0)&(training_dataset$freq_extend_Elite > 0)

training_dataset[fil,]

# For accepting Offer 

cor(cbind(training_dataset$freq_accept_Supp,training_dataset$freq_accept_Credit,training_dataset$freq_accept_Elite))

# so once again the correlation between them is very weak

# For IV extend Offer

IV_extend_mat <-cbind(training_dataset$IV_extend_Supp,training_dataset$IV_extend_Credit,training_dataset$IV_extend_Elite)
colnames(IV_extend_mat) <- c('IV_extend_Supp','IV_extend_Credit','IV_extend_Elite')

colnames(IV_extend_mat)
cor(IV_extend_mat)


fil <- (training_dataset$IV_extend_Supp == 0)&(training_dataset$IV_extend_Credit > 0)&(training_dataset$IV_extend_Elite > 0)

mean(fil)

training_dataset[fil,]

# For IV Accept Offer

IV_accept_mat <- cbind(training_dataset$IV_accept_Supp,training_dataset$IV_accept_Credit,training_dataset$IV_accept_Elite)
cor(IV_accept_mat)

fil <- (training_dataset$IV_accept_Supp == 0)&(training_dataset$IV_accept_Credit > 0)&(training_dataset$IV_accept_Elite > 0)

mean(fil)


complete_IV_mat <- cbind(IV_extend_mat,IV_accept_mat)

cor(complete_IV_mat)



