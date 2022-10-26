library('tree')
library(xtable)
setwd("/Users/christoffergardin/Desktop/TDDE01/Lab2")

#1.
#Data pre-processing, set as factor for the features that are categorical and drop duration column.
data <- read.csv('bank-full.csv', header = TRUE, sep = ";", stringsAsFactors = T)
df <- data[-12]
#Split into training/validation/test as 40/30/30
n = dim(df)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.4))
train = df[id,]
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.3))
valid = df[id2,]
id3 = setdiff(id1,id2)
test = df[id3,]

#Misclassification function
missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n) }

#2a)
#Decision tree with default settings
tree.a <- tree(y ~ ., method = "class", data = train)
pred.valid_tree.a <- predict(tree.a,valid,type="class") #Prediction on valid data sample
pred.train_tree.a <- predict(tree.a,train,type="class") #Prediction on train data sample
#Misclassification rates
miss.valid_tree.a <- missclass(valid$y,pred.valid_tree.a) 
miss.train_tree.a <- missclass(train$y,pred.train_tree.a)
cat(paste("train_tree.a misclassification rate: "), miss.train_tree.a ,"\n")
cat(paste("valid_tree.a misclassification rate: "), miss.valid_tree.a,"\n")

plot(tree.a)
text(tree.a, pretty = 0)
summary(tree.a)
tree.a

#2b)
#Decision tree with smallest allowed node size equal to 7 000
tree.b <- tree(y ~ ., method = "class", data = train, control = tree.control(nobs = nrow(train), minsize = 7000))
pred.valid_tree.b <- predict(tree.b,valid,type="class") #Prediction on valid data sample
pred.train_tree.b <- predict(tree.b,train,type="class") #Prediction on train data sample
#Misclassification rates
miss.valid_tree.b <- missclass(valid$y,pred.valid_tree.b)
miss.train_tree.b <- missclass(train$y,pred.train_tree.b)
cat(paste("train_tree.b misclassification rate: "), miss.train_tree.b ,"\n")
cat(paste("valid_tree.b misclassification rate: "), miss.valid_tree.b,"\n")

plot(tree.b)
text(tree.b, pretty = 0)
summary(tree.b)

#2c)
#Decision tree with minimum deviance 0.0005
tree.c <- tree(y ~ ., method = "class", data = train, control = tree.control(nobs = nrow(train), mindev = 0.0005))
pred.valid_tree.c <- predict(tree.c,valid,type="class") #Prediction on valid data sample
pred.train_tree.c <- predict(tree.c,train,type="class") #Prediction on train data sample
#Misclassification rates
miss.valid_tree.c <- missclass(valid$y,pred.valid_tree.c)
miss.train_tree.c <- missclass(train$y,pred.train_tree.c)
cat(paste("train_tree.b misclassification rate: "), miss.train_tree.c ,"\n")
cat(paste("valid_tree.b misclassification rate: "), miss.valid_tree.c,"\n")

plot(tree.c,type="uniform")
text(tree.c, pretty = 0)
summary(tree.c)

#3
#Init deviance score vector for training set and valid set
train.score <- rep(0,50)
valid.score <- rep(0,50)

#Pruning and studying the trees up to 50 leaves
for(i in 2:50) {
  pruned.tree <- prune.tree(tree.c, best = i)
  pred.v.tree <- predict(pruned.tree, valid, type = "tree")
  train.score[i] <- deviance(pruned.tree)
  valid.score[i] <- deviance(pred.v.tree)
}

#Optimal nr. of leafs: 22 (2:50 has one less element than valid score hence 21 + 1)
opt.leaf <- (which.min(valid.score[2:50])) + 1 
plot(2:50, train.score[2:50], type = "b", col = "red", xlim = c(0,50), ylim = c(8000,12000), xlab = "Number of leafs", ylab = "Deviance")
points(2:50, valid.score[2:50], type = "b", col = "blue")
legend(35, 12000, legend=c("Training score", "Valid score"),
       col=c( "red", "blue"), cex=0.7, pch = 1)

#Final tree with 22 leafs(terminal nodes)
final.tree <- prune.tree(tree.c, best = opt.leaf)
pred.final <- predict(final.tree, valid, type = "class")
table.final.valid <- table(valid$y,pred.final)
plot(final.tree)
text(final.tree, pretty = 0)
summary(final.tree)

#Task 4
#Prediction on the final tree with 22 leafs and the test data set
pred.final.test <- predict(final.tree, test, type = "class")
table.final <- table(test$y,pred.final.test)

acc <- (table.final[2,2]+table.final[1,1])/(sum(table.final)) #accuracy = (TP+TN)/(Total)
prec <- (table.final[2,2])/(table.final[2,2]+table.final[1,2]) #precision = (TP)/(TP+FP)
recall <- (table.final[2,2])/(table.final[2,2]+table.final[2,1]) #recall = (TP)/(TP+FN)
f1.score <- (2*prec*recall)/(prec+recall) # F1-score = (2*precision*recall)/(precision+recall)

cat(paste("Final tree accuracy: "), acc,"\n")
cat(paste("Final tree precision: "), prec,"\n")
cat(paste("Final recall: "), recall,"\n")
cat(paste("Final tree F1.score: "), f1.score,"\n")
print(table.final)

#Task 5
#Prediction on the final tree with 22 leafs, the test data, and a loss matrix
pred.loss_matrix <- predict(final.tree, test, type = "vector")
pred.loss_matrix_1 <- transform(pred.loss_matrix, value = no / yes)
pred.loss_matrix_2 <- ifelse(pred.loss_matrix_1[,3] > (5/1),"no","yes") #for example if we would had a loss matrix (0 5 2 0) we would take >= 5/2
table.loss_matrix <- table(test$y,pred.loss_matrix_2)
print(table.loss_matrix)

acc1 <- (table.loss_matrix[2,2]+table.loss_matrix[1,1])/(sum(table.loss_matrix)) #accuracy = (TP+TN)/(Total)
prec1 <- (table.loss_matrix[2,2])/(table.loss_matrix[2,2]+table.loss_matrix[1,2]) #precision = (TP)/(TP+FP)
recall1 <- (table.loss_matrix[2,2])/(table.loss_matrix[2,2]+table.loss_matrix[2,1]) #recall = (TP)/(TP+FN)
f1.score1 <- (2*prec1*recall1)/(prec1+recall1) # F1-score = (2*precision*recall)/(precision+recall)

#Task 6
#Logistic regression model 
glm.model <- glm(y ~., data = train, family = binomial)
glm.pred <- predict(glm.model, test, type = "response")

#Probabilistic values for the final tree
pred.opt.test <- predict(final.tree, test, type = "vector")

#Init vectors for TPR and FPR
tpr.tree <- rep(0,18)
tpr.glm <- rep(0,18)
fpr.tree <- rep(0,18)
fpr.glm <- rep(0,18)

#Calculating FPR and TPR for both the logistic regression model and the tree model
i = 0.05
j = 1
while (i < 1) {
  glm.res <- ifelse(glm.pred > i,"yes","no")
  pred.final.test <- ifelse(pred.opt.test[,2] > i,"yes","no")
  tree.table <- table(test$y,pred.final.test) 
  glm.table <- table(test$y,glm.res)
  
  #For large values on i the tree model does not have a "yes" column
  if(tree.table[1,1]+tree.table[2,1] == nrow(pred.opt.test)) {
    tpr.tree[j] <- 0
    tpr.glm[j] <- (glm.table[2,2])/(glm.table[2,1] + glm.table[2,2])
    fpr.tree[j] <- 0
    fpr.glm[j] <- (glm.table[1,2])/(glm.table[1,1] + glm.table[1,2])
  }
  else {
    tpr.tree[j] <- (tree.table[2,2])/(tree.table[2,1] + tree.table[2,2])
    tpr.glm[j] <- (glm.table[2,2])/(glm.table[2,1] + glm.table[2,2])
    fpr.tree[j] <- (tree.table[1,2])/(tree.table[1,1] + tree.table[1,2])
    fpr.glm[j] <- (glm.table[1,2])/(glm.table[1,1] + glm.table[1,2])
  }
  i = i + 0.05
  j = j + 1
} 

plot(fpr.glm, tpr.glm, col = "blue", type = "b",xlim = c(0,1), ylim = c(0,1), xlab = "False Positive Rate", ylab = "True Positive Rate")
points(fpr.tree, tpr.tree, type = "b", col = "red")
legend(0.05, 1, legend=c("Logistic regression model", "Tree model"),
       col=c( "blue", "red"), cex=0.7, pch = 1)

