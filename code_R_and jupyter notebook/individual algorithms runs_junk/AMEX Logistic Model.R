
# Use file AMEX Data Builder to build Train datasets


# Libraries

library(nnet)



mydata = mydata_G

str(mydata)

table(mydata$Offer.Accepted)

# "No Offer" as reference level

mydata$Offer.Accepted.Ref <- relevel(mydata$Offer.Accepted,ref='No Offer')

table(mydata$Offer.Accepted.Ref)

mean(mydata$Offer.Accepted.Ref != mydata$Offer.Accepted)
head(mydata)


# Develop Multinomial Logistic Model

mymodel <- multinom( Offer.Accepted.Ref ~ . -cm_key - Offer.Accepted , data = mydata, maxit = 300)
summary(mymodel)


# Predict

predict(mymodel,mydata)
predict(mymodel,mydata,type = 'prob')
predict(mymodel,mydata[c(3,100,400),],type = 'prob')





# Misclassific Error

cm <- table(predict(mymodel),mydata$Offer.Accepted.Ref)
cm


1-sum(diag(cm))/sum(cm)


# Two Tailed z test
c <- summary(mymodel)
z2 <- c$coefficients/c$standard.errors
p2 <- (1- pnorm(abs(z2),0,1)) * 2
p2
