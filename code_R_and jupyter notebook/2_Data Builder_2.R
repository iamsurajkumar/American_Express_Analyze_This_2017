
fd <- read.csv('Final_Dataset.csv')
names(fd)


fd_names <- c('cm_key','card.type','fam.size'
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

length(fd_names)
length(names(fd))

colnames(fd) <- fd_names

head(fd,10)
str(fd)

# Factor Conversion

lb$plcard <- as.factor(lb$plcard)

# Checking the lb for missing values

sum(fd$income == 0)
sum(fd$cust.spend.cap == 0)
fd$income[fd$income == 0] = NA
fd$cust.spend.cap[fd$cust.spend.cap == 0] = NA

#Imputing the Missing values for lb for Income and Cust.Spend.Cap
library(DMwR)
myfd <- knnImputation(fd)  # perform knn imputation.
anyNA(myfd)
anyNA(fd)





# Combining the Quarter Variables

myfd$elec <- myfd$elec_q1 + myfd$elec_q2+myfd$elec_q3 + myfd$elec_q4
myfd$trav <- myfd$trav_q1 + myfd$trav_q2 + myfd$trav_q3+myfd$trav_q4
myfd$hou <- myfd$hou_q1+ myfd$hou_q2+myfd$hou_q3+myfd$hou_q4
myfd$car <- myfd$car_q1 + myfd$car_q2+ myfd$car_q3+myfd$car_q4 
myfd$retail <- myfd$retail_q1 +myfd$retail_q2+myfd$retail_q3+ myfd$retail_q4
myfd$total.spend <- myfd$tot_q1 +myfd$tot_q2+myfd$tot_q3+myfd$tot_q4


head(myfd)
names(myfd.sub)
names(lb.mydata)
str(myfd.data)

drop.vars <- c('card.type','cust.spend.cap',
               'elec_q1','elec_q2','elec_q3','elec_q4',
               'trav_q1','trav_q2','trav_q3','trav_q4',
               'hou_q1','hou_q2','hou_q3','hou_q4',
               'car_q1','car_q2','car_q3','car_q4',
               'retail_q1','retail_q2','retail_q3','retail_q4',
               'tot_q1','tot_q2','tot_q3','tot_q4')

# 
myfd.sub <- myfd[,!names(myfd) %in% drop.vars]


myfd.data <- myfd.sub

str(myfd.data)
names(myfd.data)

myfd.data$plcard = as.factor(myfd.data$plcard)

myfd.data