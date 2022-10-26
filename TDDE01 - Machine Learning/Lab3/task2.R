# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58]) #scale all columns except the predicted column type
tr <- spam[1:3000, ] #training: first 3 000 rows
va <- spam[3001:3800, ] #validation: rows between 3 001 and 3 800
trva <- spam[1:3800, ] #training and validation: first 3 800 rows
te <- spam[3801:4601, ] #test: rows between 3 801 and 4 601

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  print(i)
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

#Filter 0 is trained on the training data set and tested in the validation data set
filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t) #misclassification error
err0

#Filter 1 is the best: is trained on training data and predicts on the test data
filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

#Filter 2 is trained on the training and validation data
filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t) 
err2

#Filter 3 is trained on the whole spam data set
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?

#The differences we can find between the filters is that the some are trained and predicted on different data sets.
#The most appropriate one is the one that is trained on the training data set and predicted on the test data set which is filter1.

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?

# 3. Implementation of SVM predictions.

sv<-alphaindex(filter3)[[1]] #Indexes of the of the support vectors
co<-coef(filter3)[[1]] # The linear coefficients for the support vectors
inte<- - b(filter3) #The negative intercept of the linear combination
spam.features <- spam[-58] #Remove the predicted value
lambda <- 0.05 #Tuned parameter for RBF
k<-NULL
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  k2 <- NULL
  k2<-inte
  for(j in 1:length(sv)){
    k2<- k2 + co[j]*exp(-lambda*(sum((spam.features[sv[j],]-spam.features[i,])^2))) #intercept + weights * RBF kernel function
  }
  k<-c(k,k2) #Concatenate k2
  print(k)
}
k
predict(filter3,spam[1:10,-58], type = "decision")