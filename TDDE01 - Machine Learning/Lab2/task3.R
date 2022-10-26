library(ggplot2)
library(reshape2)
#3.1
data=read.csv2("communities.csv", dec=".", sep=",")
index <- names(data) %in% c('ViolentCrimesPerPop')
X <- scale(data[,!index], center=TRUE)
X <- cbind(X, ViolentCrimesPerPop=data[,index])
N <- length(data[,1])
S = 1/N * t(X) %*% X
d = eigen(S)
lambda = d$values
sprintf("%2.3f",lambda/sum(lambda)*100)

# First 35 components will sum up to to 95% variance
sum(lambda[1:35])

# Contribuitions of PCA 1 and 2
sprintf("PCA1: %2.3f | PCA2: %2.3f", lambda[1], lambda[2])

#3.2

res <- princomp(X)
# Plot the trace for the first component
re <- prcomp(X)
U <- re$rotation
plot(sort(U[,1]), main="Traceplot, PC1", ylab="U[,1]")


df = data.frame(len=res$scores[,1])
pca1 = U[,1]
pca2 = U[,2]

# Five most contributive factors for PCA1
pca1[sort(abs(pca1), index.return=TRUE, decreasing = TRUE)$ix[1:5]]

#Plot scores for pca1 and pca2 with coloring that show Violentcrimes
df = data.frame(p1 = re$x[,1], p2=re$x[,2], c = data$ViolentCrimesPerPop)
ggplot(df, aes(x=p1, y=p2, color=c)) + 
  geom_point()


# 3.3
n=dim(data)[1]
sd = as.data.frame(scale(data))
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=as.data.frame(sd[id,]) 
test=as.data.frame(sd[-id,]) 
model = lm(ViolentCrimesPerPop ~ ., data=train)
summary(model)
pred.train = predict(model, train)
pred.test = predict(model, test)
# Train and test errors for LM
mse.train = mean((train$ViolentCrimesPerPop - pred.train)^2)
mse.test = mean((test$ViolentCrimesPerPop - pred.test)^2)


#3.4
min.mse <- function(par){
  theta <- par[1:100]
  m_train <- as.matrix(train[,1:100]) %*% matrix(theta,length(theta),1)
  m_test <- as.matrix(test[,1:100]) %*% matrix(theta,length(theta),1)
  mse.train = mean((train$ViolentCrimesPerPop - m_train)^2)
  mse.test = mean((test$ViolentCrimesPerPop - rowSums(m_test))^2)
  .GlobalEnv$k= .GlobalEnv$k+1
  .GlobalEnv$test_score[[k]] = mse.test
  .GlobalEnv$train_score[[k]] = mse.train
  return(mse.train)
}

#run regression for 
par = replicate(100, 0)
train_score = c()
test_score = c()
k = 0
r <- optim(par, min.mse, method = c("BFGS"))  

#plot test and training error

y_test = unlist(test_score[500:k])
df = data.frame(y_train=pmin(unlist(train_score[1000:k]), 0.6), y_test=pmin(unlist(test_score[1000:k]),0.6), x_=c(1000:k))
dt.df <- melt(df, measure.vars = c("y_train", "y_test"))
dt$x_ <- x_= c(1000:k)
ggplot(data = dt.df, aes(x=x_, y=value))+
  geom_line(aes(color=variable))+
  facet_grid(variable ~ ., scales = "free_y") +
  labs(x="Iteration", y = "")
theme(legend.position="none")


#minimum test and training values from early stop
min(unlist(train_score)) # task3 train: 0.2591772 task4: 0.2592247
min(unlist(test_score)) # task3 test: 0.4000579 task4: 0.3769468

st = sort(unlist(test_score), index.return=TRUE)
st$ix[1:10]
unlist(test_score)[2183]
unlist(train_score)[2183]