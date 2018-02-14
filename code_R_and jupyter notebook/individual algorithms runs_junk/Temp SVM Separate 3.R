library(e1071)
Accept.svm = svm(IV_Accept ~. -cm_key  - IV_accept_Supp - IV_accept_Elite - IV_accept_Credit, 
                 data = mydata.trim.train, 
                 kernel = 'radial', 
                 cost = 1e3,gamma = 1,
                 decision.values =T, probability =T)

summary(Accept.svm)

A <- predict(Accept.svm,mydata.trim.train)
table(predict = A, Actual = mydata.trim.train$IV_Accept)


B <- predict(Accept.svm,mydata.trim.test)
table(predict = B, Actual = mydata.trim.test$IV_Accept)

Accept.svm.Prob

Accept.svm.Obj <- predict(Accept.svm, newdata = mydata.trim.test,probability=TRUE)
Accept.svm.Mat = data.frame(attr(Accept.svm.Obj, "probabilities"))
Cat.svm.Prob.Credit <- Accept.svm.Mat$Credit
Cat.svm.Prob.Supp <- Accept.svm.Mat$Supp
Cat.svm.Prob.Elite <- Accept.svm.Mat$Elite