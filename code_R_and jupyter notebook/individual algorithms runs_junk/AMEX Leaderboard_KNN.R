#AMEX Leaderboard

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

# Combining the Quarter Variables

lb$elec <- lb$elec_q1 + lb$elec_q2+lb$elec_q3 + lb$elec_q4
lb$trav <- lb$trav_q1 + lb$trav_q2 + lb$trav_q3+lb$trav_q4
lb$hou <- lb$hou_q1+ lb$hou_q2+lb$hou_q3+lb$hou_q4
lb$car <- lb$car_q1 + lb$car_q2+ lb$car_q3+lb$car_q4 
lb$retail <- lb$retail_q1 +lb$retail_q2+lb$retail_q3+ lb$retail_q4
lb$total.spend <- lb$tot_q1 +lb$tot_q2+lb$tot_q3+lb$tot_q4
lb$Customer.Response <-  mapply(custResponse, lb$freq_extend_Supp,lb$freq_extend_Elite,lb$freq_extend_Credit,lb$freq_accept_Supp,lb$freq_accept_Elite,lb$freq_accept_Credit)


# Filterign Out useful variables for the Model
model.vars <- c('fam.size','total.active.cards','account.age',
                'cm.fees','high.spend.affinity','int.inf.score',
                'income','plcard','int.busi.exp.score','freq.payments',
                'cm.number','elec','trav','hou','car','retail',
                'Customer.Response')

# Building building the train.KNN

train.KNN <- train[,names(train) %in% model.vars]
length(names(train.KNN))



lb.KNN <- lb[,names(lb) %in% model.vars]
length(names(lb.KNN))

names(lb.KNN)


#Standardizng the modle
standardized.lb = scale(lb.KNN[,-c(1)])
head(standardized.lb)
head(standardized.mydata)

# Running the Mode
library(class)
knn.pred <- knn(standardized.mydata,standardized.lb,mydata$Offer.Accepted,k=5)


# Looking at the Results
table(knn.pred)


#Now exporting results to dataframe

result = as.data.frame(lb.KNN$cm_key)
result$predict = knn.pred

head(result,10)



# Filtering out the No Offer category in result

result_fil <- result[result$predict != 'No Offer',]
head(result_fil)


#Now writing the code to a csv file

write.csv(result_fil,'Closed&bounded_DSE_10.csv',row.names = FALSE,col.names = NA)


#Some Code
#result_1000 <- result[sample(nrow(result), 1000,replace = FALSE), ]
