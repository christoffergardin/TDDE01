#caTools used for splitting data into training and test
library(caTools)
#psych used for matrix operations
library(psych)

setwd("/Users/christoffergardin/Desktop/TDDE01/Lab1")

#1.
#reads file into data frame 
df <- read.csv('parkinsons.csv')

#scale and include the data except from the columns "subject, age, sex, test_time, Total_UPDRS"

dfscale <- scale(df[c(-1,-2,-3,-4,-6)])
dfscale <- as.data.frame(dfscale)

#split data into training (60%) and test (40%)
#sample <- sample.split(dfscale$motor_UPDRS, SplitRatio = 0.6)
#train <- subset(dfscale, sample == TRUE)
#test <- subset(dfscale, sample == FALSE)
n=dim(dfscale)[1]
set.seed(12345)
id=sample(1:n,floor(n*0.6))
train=dfscale[id,]
test=dfscale[-id,]

#2.
#create a regression model based on the training data and the predicted variable motor_UPDRS
model <- lm(motor_UPDRS ~ ., data = train )
#print(summary(model))
#print(fitted(model))

#predict motor_UPDRS 
pred.test <- predict(model,test)
pred.training <- predict(model,train)

#Calculating mean squared error test data
results.test <- cbind(pred.test, test$motor_UPDRS)
colnames(results.test) <- c('pred','real')
results.test <- as.data.frame(results.test)
mse.test <- mean((results.test$real - results.test$pred)^2)
cat(paste("MSE test: "), mse.test,"\n")

#Calculating mean squared error training data
results.training <- cbind(pred.training, train$motor_UPDRS)
colnames(results.training) <- c('pred','real')
results.training <- as.data.frame(results.training)
mse.training <- mean((results.training$real - results.training$pred)^2)
cat(paste("MSE training:"), mse.training,"\n")

print(mean((model$residuals)^2))
#3.
#a)
#computes the log-likelihood function for training data given parameters theta1 and sigma1
Loglikelihood <- function(training.x, training.y, theta1, sigma1) {
  loglik <- -(length(training.y)/2) * log(2*pi*(sigma1)^2) - (1/(2*(sigma1)^2)) * sum((training.x%*%theta1 - training.y)^2)
  #print(-loglik)
  return(-loglik) 
}

#b)
#adds a ridge penalty to the log-likelihood
Ridge <- function(training.x, training.y, par, lambda) {
  theta1 <- as.matrix(par[1:16])
  sigma1 <- as.matrix(par[17])
  ridge.loglik <- lambda*sum(theta1^2) + Loglikelihood(training.x, training.y, theta1, sigma1)
  return(ridge.loglik)
}

#c)
#optimize paramters(par) in the Ridge function 
RidgeOpt <- function(training.x, training.y, par, lambda) {
  ridge.opt <- optim(par, Ridge, training.x = training.x, training.y = training.y,lambda = lambda, method = c("BFGS"))
  return(ridge.opt)
}

#d)
#compute degrees of freedom
DF <- function(x.data, lambda) {
  df <- tr(x.data%*%solve(t(x.data)%*%x.data + lambda * diag(ncol(x.data)))%*%t(x.data))
  return(df)
}

#initializing variables

#training data, all columns except the first one
training.x = as.matrix((train[,-1]))
test.x = as.matrix((test[,-1]))
#predicted column motor_UPDRS
training.y = as.matrix((train[,1]))
test.y = as.matrix((test[,1]))
#init features
#theta = rnorm(ncol(training.x))
theta = sample(c(rep(0,8),rep(1,8)))
#init sigma
sigma = runif(1)
#parameters to optimize
par = c(theta,sigma)


#computing optimal parameters, predicting motor_UPDRS and calculating
#degrees of freedoom for lambda = 1, lambda = 100, lambda = 1000
lambdas = c(1,100,1000)
for (lambda in lambdas){
  opt.training <- RidgeOpt(training.x, training.y, par, lambda)
  opt.test <- RidgeOpt(test.x, test.y, par, lambda)
  mse.opt.training <- mean((training.x%*%opt.training$par[1:16] - training.y)^2)
  mse.opt.test <- mean((test.x%*%opt.test$par[1:16] - test.y)^2)
  df.training <- DF(training.x, lambda)
  df.test <- DF(test.x, lambda)
  
  cat("MSE opt. training, lambda = ",lambda,":", mse.opt.training, "\n")
  cat("MSE opt. test, lambda = ",lambda,":", mse.opt.test, "\n")
  cat("DF training, lambda = ",lambda,":", df.training, "\n")
  cat("DF test, lambda = ",lambda,":", df.test, "\n")
}
