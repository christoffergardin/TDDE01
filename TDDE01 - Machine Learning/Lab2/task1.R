#Lab 2
library("glmnet")
#Assignment 1
setwd("/Users/christoffergardin/Desktop/TDDE01/Lab2")
data=read.csv("tecator.csv")

#Divide into train and test
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

# 1.1
plot(density(train$Fat), main="Distribution", xlab="Fat", ylab="Density")
# Linear regression model
fat_model <- lm(Fat ~ ., train[,2:102])
pred.train = predict(fat_model, train[,2:101])
pred.test = predict(fat_model, test[,2:101])

# Calculating mean squared error training data
results.train <- cbind(pred.train, train$Fat)
colnames(results.train) <- c('Pred','Real')
results.train <- as.data.frame(results.train)
mse.train <- mean((results.train$Real - results.train$Pred)^2)
cat(paste("MSE training:"), mse.train,"\n") #Small error
print(mean((pred.test - test[,102])^2))

# Calculating mean squared error test data
results.test <- cbind(pred.test, test$Fat)
colnames(results.test) <- c('Pred','Real')
results.test <- as.data.frame(results.test)
mse.test <- mean((results.test$Real - results.test$Pred)^2)
cat(paste("MSE test: "), mse.test,"\n")

# 1.2
# J(\theta, X, Y) = (1/n) * SUM(y_i - \theta_0 - \theta_1*x_{1j} -...- \theta_p*x_{pj})^2)
# j = 1:100   All channels as features
# Cost function should be minimized

# 1.3
# LASSO regression model
lasso_model = glmnet(as.matrix(train[2:101]), train$Fat, alpha = 1)
plot(lasso_model, xvar="lambda", label=TRUE)

lasso_model$lambda
# 1.4
# Ridge regression model
ridge_model = glmnet(as.matrix(train[2:101]), train$Fat, alpha = 0)
plot(ridge_model, xvar="lambda", label=TRUE)

# 1.5
# Cross-validation of LASSO regression model
lasso_cv = cv.glmnet(as.matrix(train[2:101]), train$Fat, alpha=1)
lasso_cv$lambda.min
plot(lasso_cv)
coef(lasso_cv, a0="lambda.min") #Optimal lambda
colSums(coef(lasso_cv) != 0) #Amount of nonzero elements

# Scatterplot with y=x line for comparison
lasso_pred = predict(lasso_cv, as.matrix(test[2:101]), s = lasso_cv$lambda.min)
plot(test$Fat, lasso_pred, xlim = c(0,60), ylim = c(0,60), xlab="Test data values", ylab="Lasso model test data prediction")
abline(a=0, b=1)
