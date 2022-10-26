#Assignment 1
#missclass function
missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n) }
#Task 1.1

library(ggplot2)
library(xtable)
library(kknn)
Dataframe=read.csv("optdigits.csv", header = FALSE)
Dataframe$V65 = as.factor(Dataframe$V65)
n = dim(Dataframe)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = Dataframe[id,]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, round(n*0.25))
valid = Dataframe[id2,]

id3 = setdiff(id1, id2)
test = Dataframe[id3,]

#Task 1.2
model_1 = kknn(as.factor(V65) ~., train, test, kernel = "rectangular", k = 30)
model_2 = kknn(as.factor(V65) ~., train, train, kernel = "rectangular", k = 30)
fit <- fitted(model_1)
fit_2 <- fitted(model_2)

conf_test = table(test$V65, fit)
conf_train = table(train$V65, fit_2)
print(xtable(conf_test, type = "latex"), file = "conf_test.tex")
print(xtable(conf_train, type = "latex"), file = "conf_train.tex")
missclass(test$V65, fit) 
missclass(train$V65, fit_2) 

#Task 1.3

fig_matrix_2 = matrix(as.numeric(train[211,1:64]), 8, 8, byrow=TRUE)
heatmap(fig_matrix_2, Rowv = NA, Colv = NA)

index = 1
prob_vector = c()
index_vector = c()
for (i in fit_2)
{
  if (i == 8 && train[index,65] == 8){
    prob_vector <- c(prob_vector, model_2$prob[index,9])
    index_vector <- c(index_vector, index)
  }
  index = index + 1
}

prob_vector_sorted = sort(prob_vector, index.return=TRUE)
hard_vector = prob_vector_sorted$ix[1:3]
easy_vector = prob_vector_sorted$ix[187:188]

# Easy 8:s
fig_matrix_1 = matrix(as.numeric(train[index_vector[easy_vector[1]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_1, Rowv = NA, Colv = NA)

fig_matrix_2 = matrix(as.numeric(train[index_vector[easy_vector[2]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_2, Rowv = NA, Colv = NA)
# Hard 8:s
fig_matrix_3 = matrix(as.numeric(train[index_vector[hard_vector[1]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_3, Rowv = NA, Colv = NA)

fig_matrix_4 = matrix(as.numeric(train[index_vector[hard_vector[2]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_4, Rowv = NA, Colv = NA)

fig_matrix_5 = matrix(as.numeric(train[index_vector[hard_vector[3]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_5, Rowv = NA, Colv = NA)

# Task 1.4
error_vector_train = c()
error_vector_valid= c()
k_vector = c(1:30)
for (i in k_vector){
  model_3 = kknn(as.factor(V65) ~., train, train, kernel = "rectangular", k = i)
  fit_3 <- fitted(model_3)
  error_vector_train <- c(error_vector_train, missclass(train$V65, fit_3))  
  
  model_4 = kknn(as.factor(V65) ~., train, valid, kernel = "rectangular", k = i)
  fit_4 <- fitted(model_4)
  error_vector_valid <- c(error_vector_valid, missclass(valid$V65, fit_4))
}

plot(k_vector, error_vector_train, col = "orange",xlim = c(0,30), ylim = c(0,0.06), xlab = "k - value", ylab = "Miss-classification error")
points(k_vector, error_vector_valid, col = "blue")
legend(0, 0.06, legend=c("Validation data", "Training data"),
       col=c( "blue", "orange"), cex=0.7, pch = 1)
# Estimate test error for optimal k and compare to train and valid.
model_5 = kknn(as.factor(V65) ~., train, test, kernel = "rectangular", k = 3)
model_6 = kknn(as.factor(V65) ~., train, train, kernel = "rectangular", k = 3)
model_7 = kknn(as.factor(V65) ~., train, valid, kernel = "rectangular", k = 3)
fit_5 <- fitted(model_5) # test
fit_6 <- fitted(model_6) # train
fit_7 <- fitted(model_7) # valid

test_error = missclass(test$V65, fit_5)
train_error = missclass(train$V65, fit_6)
valid_error = missclass(valid$V65, fit_7)

#Creating error table
error_table <- matrix(c(round(test_error, digits = 4), round(train_error, digits = 4), round(valid_error, digits = 4)), ncol = 3, byrow = TRUE)
colnames(error_table) <- c("Test", "Train","Valid")
error_table <- as.table(error_table)
print(xtable(error_table, type = "latex"), file = "table_1_4.tex")
# Task 1.5

#cross entropy

crossentropy <- function(p, phat){
  x <- 0
  for (i in 1:length(p)){
    x <- x + log(phat[i,p[i]] + 1e-15)
  }
  return (-x)
}

HP_val= c(NULL)
HP_test= c(NULL)
for (i in 1:30) {
  model_8 <- kknn(V65 ~ ., train, valid, kernel = "rectangular", k = i)
  fit_8 <- fitted(model_8)
  HP_val = append(HP_val,crossentropy(as.numeric(valid$V65), model_8$prob))
}
hp_data = data.frame(
  sup=rep(c("Val"), each=30), 
  len=c(HP_val, HP_test), 
  val=rep(c(1:30),1))

barplot(hp_data$len, axisnames = TRUE, names.arg = hp_data$val, ylim = c(0, 1000), xlab = "k - value", ylab = "Cross - entropy error", col = "orange")
grid(nx = 0, ny = 5)


