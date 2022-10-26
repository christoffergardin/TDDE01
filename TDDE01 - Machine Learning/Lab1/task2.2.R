#psych used for matrix operations
library(psych)
setwd("/Users/christoffergardin/Desktop/TDDE01/Lab1")

df <- read.csv('parkinsons.csv')

df.scale <- scale(df[c(-1,-2,-3,-4,-6)])
df.scale <- as.data.frame(df.scale)

n <- dim(df.scale)[1] #Gets the number of rows, [2] gives columns
set.seed(12345)
id <- sample(1:n,floor(n*0.6)) #Gives 60% random samples
train <- df.scale[id,]
test <- df.scale[-id,]

model.1 <- lm(motor_UPDRS ~ ., data = train)

pred.test1 <- predict(model.1, test)
pred.train1 <- predict(model.1, train)

results.test1 <- cbind(pred.test1, test$motor_UPDRS)
colnames(results.test1) <- c('pred','real')
results.test1 <- as.data.frame(results.test1)
mse.test1 <- mean((results.test1$real - results.test1$pred)^2)

mse.test11 <- mean((pred.test1 - test$motor_UPDRS)^2) #also works

results.train1 <- cbind(pred.train1, train$motor_UPDRS)
colnames(results.train1) <- c('pred','real')
results.train1 <- as.data.frame(results.train1)

mse.train1 <- mean((-results.train1$real + results.train1$pred)^2)
print(mse.train1)
Loglikelihood <- function(training.x, training.y, theta1, sigma1) {
  loglik <- -(length(training.y)/2) * log(2*pi*(sigma1)^2) - (1/(2*(sigma1)^2)) * sum((training.x%*%theta1 - training.y)^2)
  return(-loglik)
}

Ridge <- function(training.x, training.y, par, lambda) {
  theta1 <- as.matrix(par[1:16])
  sigma1 <- as.matrix(par[17])
  ridge.loglik <- lambda*sum(theta1^2) + Loglikelihood(training.x, training.y, theta1, sigma1)
}

RidgeOpt <- function(training.x, training.y, par, lambda) {
  ridge.opt <- optim(par, Ridge, training.x = training.x, training.y = training.y, lambda = lambda, method = c("BFGS"))
  return(ridge.opt)
}

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


