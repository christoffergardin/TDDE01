library(ggplot2)

data=read.csv("pima-indians-diabetes.csv", header=FALSE)
d = data.frame(glucos=data$V2, age=data$V8, diabetes=data$V9)

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

#3.1
p <- qplot(age, glucos, data=d, color=factor(diabetes))+
  xlab("Age (x1)")+
  ylab("Glucose (x2)")
p$labels$colour <- "Diabetes"
p
# 3.2
glm.fit <- glm(diabetes ~ glucos+age, data=d,family=binomial)
glm.probs <- predict(glm.fit, newdata = d, type="response")
glm.pred <- ifelse(glm.probs > 0.5, "1", "0")

p1 <- qplot(age, glucos, data=d, color=glm.pred)+
  xlab("Age (x1)")+
  ylab("Glucose (x2)")
p1$labels$colour <- "Diabetes"
p1
missclass(d$diabetes, glm.pred) # 0.2630208

# 3.3
coeffs = coef(glm.fit) 
slope = -coeffs[3]/coeffs[2] #Ex decision bundary: x*0.02477835 + y*0.03564404 - -5.91244906 = 0 
int = -coeffs[1]/coeffs[2]   # --> y = (5.91244906/0.03564404) - ((x*0.02477835)/0.03564404) = 166 - x*0.68
                             #Where y = Glucose(x2) and x = Age(x1)

p2 <- ggplot(d, aes(x=age, y=glucos, color=glm.pred)) + geom_point() + 
  geom_abline(intercept = int, slope = slope, color ="pink")+
  xlab("age (x2)")+
  ylab("glucus (x1)")
p2$labels$colour <- "Diabetes"
p2
#3.4
glm.pred <- ifelse(glm.probs > 0.2, "1", "0")
p4<- qplot(age, glucos, data=d, color=glm.pred)+
  xlab("Age (x1)")+
  ylab("Glucos (x2)")
p4$labels$colour <- "Diabetes"
p4
glm.pred <- ifelse(glm.probs > 0.8, "1", "0")
p5<- qplot(age, glucos, data=d, color=glm.pred)+
  xlab("Glucose (x1)")+
  ylab("Age (x2)")
p5$labels$colour <- "Diabetes"
p5
# We see that the when we change r the slope remains the same 
# but the intercept changes, thus requiring the model to be more
# or less sure if the person has diabetes or not.

#3.5
z_data = data.frame(z1=d$age**4, 
                    z2=(d$age**3)*(d$glucos**1), 
                    z3=(d$age**2)*(d$glucos**2), 
                    z4=d$age*d$glucos**3, 
                    z5=d$glucos**4)
basis_data <- cbind(d, z_data)

glm.fit <- glm(diabetes ~ glucos+age+z1+z2+z3+z4+z5, 
               data=basis_data, family=binomial)
glm.probs <- predict(glm.fit, newdata = basis_data, type="response")
glm.pred <- ifelse(glm.probs > 0.5, "1", "0")

p6 <- qplot(age, glucos, data=basis_data, color=glm.pred)+
  xlab("Glucose (x1)")+
  ylab("Age (x2)")
p6$labels$colour <- "Diabetes"
p6
missclass(basis_data$diabetes, glm.pred)  #  0.2447917
# We get a polynomial regression classification instead, 
# the missclassification is increased which could be because of
# overfitting from the high degree of polynomial used.
