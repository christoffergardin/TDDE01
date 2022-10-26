# Lab 3
# Assignment 3

library(neuralnet)
set.seed(1234567890)

# 3.1
sample <- runif(500, min =0, max=10) # 500, random vector 0-10
sin_sample <- sin(sample) #  Applying sine-function to the vector
data <- as.data.frame(cbind(sample, sin_sample))

# Dividing into training and test data with proportion 25:475
n=dim(data)[1]
id=sample(1:n, floor(n*0.05))
train=data[id,]
test=data[-id,]

# Creating neuralnet model with 1 layer and 10 hidden units.
model_1 = neuralnet(sin_sample ~ sample ,data = train, hidden = 10) #If we have have a classification problem then set linear.output = FALSE
# The activation function is only in the hidden layer, if act.func should not be applied to the out put we set linear.output = FALSE
# Plotting train-, test-data and test predictions.
plot(train, cex=2, xlab = "x", ylab = "Sin(x)", xlim=c(-0.1,10.1), ylim =c(-1.5,1.5))
points(test, col = "blue", cex=1)
points(test[,1], predict(model_1, test), col="red", cex=1)
legend(0, -0.3, c("Train-data", "Test-data", "Predictions"),cex = 0.9, col=c("black","blue","red"),pch=c(1,1,1))

# 3.2 Creating neuralnet models with different activation functions.
h1 <- function(x) x # Linear
model_h1 = neuralnet(sin_sample ~ sample ,data = train, hidden = 10, act.fct = h1)
h2 <- function(x) ifelse(x>=0, x, 0) # ReLU
model_h2 = neuralnet(sin_sample ~ sample ,data = train, hidden = 10, act.fct = h2)
h3 <- function(x) log(1 + exp(x)) # Softplus
model_h3 = neuralnet(sin_sample ~ sample ,data = train, hidden = 10, act.fct = h3)

# Plot with Linear activation function
plot(train, cex=2, xlab = "x", ylab = "Sin(x)",xlim=c(-0.1,10.1), ylim =c(-1.5,1.5))
points(test, col = "blue", cex=1)
points(test[,1], predict(model_h1, test), col="red", cex=1)
legend(0, -0.2, c("Train-data", "Test-data", "Predictions"),cex = .9, col=c("black","blue","red"),pch=c(1,1,1))

# Plot with ReLU activation function
plot(train, cex=2, xlab = "x", ylab = "Sin(x)",xlim=c(-0.1,10.1), ylim =c(-1.5,2))
points(test, col = "blue", cex=1)
points(test[,1], predict(model_h2, test), col="red", cex=1)
legend(0, -0.2, c("Train-data", "Test-data", "Predictions"),cex = .9, col=c("black","blue","red"),pch=c(1,1,1))

# Plot with Softplus activation function
plot(train, cex=2, xlab = "x", ylab = "Sin(x)", xlim=c(-0.1,10.1), ylim =c(-1.5,1.5))
points(test, col = "blue", cex=1)
points(test[,1], predict(model_h3, test), col="red", cex=1)
legend(0, -0.2, c("Train-data", "Test-data", "Predictions"),cex = .9, col=c("black","blue","red"),pch=c(1,1,1))

# 3.3
set.seed(1234567890)
sample <- runif(500, min =0, max=50) # 500, random vector 0-50
sin_sample <- sin(sample) #  Applying sin-function to the vector
data_2 <- as.data.frame(cbind(sample, sin_sample))

plot(data_2, cex=2, xlab = "x", ylab = "Sin(x)", ylim=c(-10,1.5), col="blue")
points(data_2[,1], predict(model_1, data_2), col="red", cex=1)
legend(0, -5, c("Test-data", "Predictions"),cex = .9, col=c("blue","red"),pch=c(1,1))

# 3.4
model_1$weights #Large input times weight and bias, if negative get squeezed and zero in the hidden layer.
#Big value and positive after wirght and bias get squeezed to one and later mult. with the last weight and bias. 
#For Big values it sums upp to -10.
plot(model_1) #The blue is the bias for each neuron. Black is weight for each neuron.
#For example, the first values are the bias and withst for each node. Next come fore the second neuron. 
# 3.5 
set.seed(1234567890)
sample_3 <- runif(500, min =0, max=10) # 500, random vector 0-10
sin_sample_3 <- sin(sample_3) #  Applying sine-function to the vector
data_3 <- as.data.frame(cbind(sample_3, sin_sample_3))

model_3 = neuralnet(sample_3 ~ sin_sample_3 ,data = data_3, hidden = 10, threshold = 0.1)

plot(data_3, cex=1, xlab = "x", ylab = "Sin(x)", ylim=c(-1.5,1.5))
points(predict(model_3, data_3), data_3[,2] , col="red", cex=1)
legend(0, -0.4, c("Train-data", "Predictions"),cex = .9, col=c("black","red"),pch=c(1,1))



#Parameter threshold set the stopping criteria for gradient descent. E.g: 0.1

##Error calcualtions:

#nn$net.result[[1]]
#nn1 = ifelse(nn$net.result[[1]]>0.5,1,0) for classification and linear output false

mse = mean((model_1$net.result[[1]]-train[,2])^2)







