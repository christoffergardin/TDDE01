setwd("/Users/christoffergardin/Desktop/TDDE01/Lab1")
library(kknn)
df <- read.csv("optdigits.csv", header = FALSE)

n = dim(df)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = df[id,] # 50% of the data

id1 = setdiff(1:n, id) #the other 50% of the data
set.seed(12345)
id2 = sample(id1, round(n*0.25)) #Pick 25% number of samples from the 50 split
valid = df[id2,]

id3 = setdiff(id1, id2) #Pick the other 25%
test = df[id3,]

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n) 
}

model.1 <- kknn(as.factor(V65) ~.,train, train, kernel = "rectangular", k = 30)
model.2 <- kknn(as.factor(V65)~.,train, test, kernel = "rectangular", k = 30)

fit.1 <- fitted(model.1)
fit.2 <- fitted(model.2)
df1 <- summary(model.2) #Gives the probabilities for the fit, the fit column is the right value

table.1 <- table(train$V65, fit.1)
table.2 <- table(test$V65, fit.2)

missclass.1 <- missclass(train$V65, fit.1)
missclass.2 <- missclass(test$V65, fit.2)



index = 1
prob.vector = c()
index.vector = c() #Index vector for data sample in training set
for (i in fit.1)
{
  if (i == 8 && train[index,65] == 8){
    prob.vector <- c(prob.vector, model.1$prob[index,9])
    index.vector <- c(index.vector, index)
  }
  index = index + 1
}

#We have a sorted vector with probabilities of right predicted eights.
#Hard and easy probabilities are stored as indicies in the sorted probability vector.
#We have also stored the indicies from which data samples in the training data set this sample were.
#So by taking train[index_vector[easy_vector[1]],1:64]), we get the right data row sample by taking the indicies in the probabilityvector and 
#after that the this index represent the right index in

prob.vector.sorted = sort(prob.vector, index.return=TRUE)
hard.vector = prob.vector.sorted$ix[1:3]
easy.vector = prob.vector.sorted$ix[187:188]

# Easy 8:s
fig_matrix_1 = matrix(as.numeric(train[index.vector[easy.vector[1]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_1, Rowv = NA, Colv = NA)

fig_matrix_2 = matrix(as.numeric(train[index.vector[easy.vector[2]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_2, Rowv = NA, Colv = NA)
# Hard 8:s
fig_matrix_3 = matrix(as.numeric(train[index.vector[hard.vector[1]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_3, Rowv = NA, Colv = NA)

fig_matrix_4 = matrix(as.numeric(train[index.vector[hard.vector[2]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_4, Rowv = NA, Colv = NA)

fig_matrix_5 = matrix(as.numeric(train[index.vector[hard.vector[3]],1:64]), 8, 8, byrow = TRUE)
heatmap(fig_matrix_5, Rowv = NA, Colv = NA)

error.train <- c()
error.valid <- c()
k.vector <- c(1:30)
for(i in k.vector) {
  model.3 <- kknn(as.factor(V65) ~.,train, train, kernel = "rectangular", k = i)
  fit.3 <- fitted(model.3)
  error.train <- c(error.train, missclass(train$V65, fit.3))
  
  model.4 <- kknn(as.factor(V65) ~., train, valid, kernel = "rectangular", k = i)
  fit.4 <- fitted(model.4)
  error.valid <- c(error.valid, missclass(valid$V65, fit.4))
  
}

plot(k.vector, error.train, col = "orange", xlim = c(0,30), ylim = c(0,0.06))
points(k.vector, error.valid, col = "blue")

model.5 <- kknn(as.factor(V65)~. ,train, test, kernel = "rectangular", k = 3)
fit.5 <- fitted(model.5)
m.5 <- missclass(test$V65, fit.5)

print(m.5)


crossentropy <- function(p, phat){ #High values, for example probability 1 + 1e-15 is in log very close to zero, so by taking the log we can take the probabilities in consideration, and the predictions with highest probabilities will give the lowest cross entropy error.
  x <- 0                           #We only have probabilities between 0 and 1 so low probabilities for example 0.05 is in log(0.05) = -3 and will incrase the loss, then in the end we take times minus so we can see the table. So in this case k = 6 have predictions with the highest probabilities.
  y <- 0
  for (i in 1:length(p)){
    #print(phat[i,p[i]+1]) #Add one, because p[i] can be number zero (0 is the right prediction)
   
    x <- x + log(phat[i,p[i]+1] + 1e-15)
    #log of all the rows in the prob model and the column is the right class label in the validation data
  }                                    #So, by summning the log for all probabilities and taking the minus
  print(x)                            #we get that k which sum upp with high probabilities 
  return (-x)
 
}

HP_val= c(NULL)
HP_test= c(NULL)
for (i in 1:30) {
  model_8 <- kknn(as.factor(V65) ~ ., train, valid, kernel = "rectangular", k = i)
  fit_8 <- fitted(model_8)
  
  HP_val = append(HP_val,crossentropy(valid$V65, model_8$prob))
}

hp_data = data.frame(
  sup=rep(c("Val"), each=30), 
  len= HP_val, #Y-axis
  val=rep(c(1:30))) #x-axis

barplot(hp_data$len, axisnames = TRUE, names.arg = hp_data$val, ylim = c(0, 1000), xlab = "k - value", ylab = "Cross - entropy error", col = "orange")
grid(nx = 0, ny = 5)

