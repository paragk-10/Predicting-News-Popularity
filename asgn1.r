rm(list=ls(all=TRUE))
library(data.table) 
library(gradDescent) 
library(caret) 
library(bigmemory)
library(car) 
library(bigalgebra)
options(bigalgebra.mixed_arithmetic_returns_R_matrix=FALSE)

# Importing the context
context = fread('OnlineNewsPopularity.csv')
context1 = context[,-c(1,2,38,39)]
summary(context1)
context1=context1[!context1$n_unique_tokens==701,]
summary(context1)
context2 <- as.data.frame(context1)

boolean <- TRUE  
if (boolean) {
  for (colName in names(context2)) {
    
    if(class(context2[,colName]) == 'integer' | class(context2[,colName]) == 'numeric') {
      
      context2[,colName] <- scale(context2[,colName])
    }
  }
}

set.seed(100)
split <- createDataPartition(context2$shares, p = 0.70, list = F)
training_seting_set <- context2[split, ]
test_set_set  <- context2[-split, ]

# running linear model using lm
model1 <- lm(training_seting_set$shares~., data=training_seting_set)
summary(model1) 

#Gradient descent function
gradient_desc <- function(training_seting_set, alpha=0.1, maxIter=10, seed=NULL){
  training_seting_set <- matrix(unlist(training_seting_set), ncol=ncol(training_seting_set), byrow=FALSE)
  set.seed(seed)
  training_seting_set <- training_seting_set[sample(nrow(training_seting_set)), ]
  set.seed(NULL)
  theta <- theta_list(ncol(training_seting_set), seed=seed)
  training_seting_set <- cbind(1, training_seting_set)
  context11 <- training_seting_set[,1:ncol(training_seting_set)-1]
  context12 <- training_seting_set[,ncol(training_seting_set)]
  Theta2 <- matrix(ncol=length(theta), nrow=1)
  updateRule <- matrix(0, ncol=length(theta), nrow=1)
  rwlength <- nrow(training_seting_set)
  for(iteration in 1:maxIter){
    error <- (context11 %*% t(theta)) - context12
    for(column in 1:length(theta)){
      term <- error * context11[,column] 
      gradient <- sum(term) / rwlength
      updateRule[1,column] <- 0.05*updateRule[1,column] + (alpha*gradient)
      Theta2[1,column] = theta[1,column] - updateRule[1,column] 
    }
    theta <- Theta2
  }
  ret <- theta
  return(ret)
}

theta_list <- function(col, min=0, max=1, seed=NULL){
  set.seed(seed)
  thetas <- runif(col, min=min, max=max)
  set.seed(seed)
  ret <- matrix(unlist(thetas), ncol=col, nrow=1, byrow=FALSE)
  return(ret)
}

# running linear model using gradient_desc function
model2 <- gradient_desc(training_seting_set, alpha = 0.2, maxIter = 1000, seed = 1)
cbind(model1$coefficients, as.vector(model2))

#### Experiment taking all variables
iter1 <- c(1,10,100,1000)
alpha1 <- rep(0.1,4)
mod1 <- vector(mode="list", length=4)
rmse1 <- vector(length=4)
rmst1 <- vector(length=4)
for(i in 1:4) { 
  mod1[[i]] <- gradient_desc(training_seting_set, alpha = alpha1[i], maxIter = iter1[i], seed = 100)
  rmse1[i] <- RMSE(gradDescent::prediction(mod1[[i]],training_seting_set[,-55]),training_seting_set$shares)
  rmst1[i] <- RMSE(gradDescent::prediction(mod1[[i]],test_set_set[,-55]),test_set_set$shares)
}
rmse1
rmst1
plot(iter1,rmse1,type="l")

# Changing the learning rate
alpha2 <- c(0.01,0.1,1,10)
iter2 <- rep(500,4)
mod2 <- vector(mode="list", length=4)
rmse2 <- vector(length=4)
rmst2 <- vector(length=4)
for(i in 1:4) { 
  mod2[[i]] <- gradient_desc(training_seting_set, alpha = alpha2[i], maxIter = iter2[i], seed = 100)
  rmse2[i] <-RMSE(gradDescent::prediction(mod1[[i]],training_seting_set[,-55]),training_seting_set$shares)
  rmst2[i] <- RMSE(gradDescent::prediction(mod2[[i]],test_set_set[,-55]),test_set_set$shares)
}
rmse2
rmst2
plot(iter2,rmse2)

#### Experiment taking the last 10 variables
context3 <- context2[,47:57]
training_set3 <- context3[split, ]
test_set3  <- context3[-split, ]

# variables/matrices/vectors for experiments
iter3 <- c(1,10,100,1000)
alpha3 <- rep(0.1,4)
mod3 <- vector(mode="list", length=4)
rmse3 <- vector(length=4)
rmst3 <- vector(length=4)
for(i in 1:4) { 
  t01 <- Sys.time()
  mod3[[i]] <- gradient_desc(training_set3, alpha = alpha3[i], maxIter = iter3[i], seed = 1)
  rmse3[i] <- RMSE(gradDescent::prediction(mod3[[i]],training_set3[,-11]),training_set3$shares)
  rmst3[i] <- RMSE(gradDescent::prediction(mod3[[i]],test_set3[,-11]),test_set3$shares)
}
rmse3
rmst3
plot(iter3,rmse3)

alpha4 <- c(0.01,0.1,1,10)
iter4 <- rep(750,4)
mod4 <- vector(mode="list", length=4)
rmse4 <- vector( length=4)
rmst4 <- vector( length=4)
for(i in 1:4) { 
  mod4[[i]] <- gradient_desc(training_set3, alpha = alpha4[i], maxIter = iter4[i], seed = 1)
  rmse4[i] <-RMSE(gradDescent::prediction(mod4[[i]],training_set3[,-11]),training_set3$shares)
  rmst4[i] <- RMSE(gradDescent::prediction(mod4[[i]],test_set3[,-11]),test_set3$shares)
}
rmse4
rmst4
plot(iter4,rmse4)

#### Experiment taking 10 chosen variables
colnames(context2)
context4 <- context2[,c(6,8,9,11,16,30,31,38,43,44,57)]
training_set4 <- context4[split, ]
test_set4  <- context4[-split, ]

# variables/matrices/vectors for experiments
iter5 <- c(1,10,100,1000)
alpha5 <- rep(0.1,4)
mod5 <- vector(mode="list", length=4)
rmse5 <- vector(length=4)
rmst5 <- vector(length=4)
for(i in 1:4) { 
  mod5[[i]] <- gradient_desc(training_set4, alpha = alpha5[i], maxIter = iter5[i], seed = 1)
  rmse5[i] <- RMSE(gradDescent::prediction(mod5[[i]],training_set4[,-11]),training_set4$shares)
  rmst5[i] <- RMSE(gradDescent::prediction(mod5[[i]],test_set4[,-11]),test_set4$shares)
}
rmse5
rmst5
plot(iter5,rmse5)

alpha6 <- c(0.01,0.1,1,10)
iter6 <- rep(750,4)
mod6 <- vector(mode="list", length=4)
rmse6 <- vector( length=4)
rmst6 <- vector( length=4)
for(i in 1:4) { 
  mod6[[i]] <- gradient_desc(training_set4, alpha = alpha6[i], maxIter = iter6[i], seed = 1)
  rmse6[i] <-RMSE(gradDescent::prediction(mod6[[i]],training_set4[,-11]),training_set4$shares)
  rmst6[i] <- RMSE(gradDescent::prediction(mod6[[i]],test_set4[,-11]),test_set4$shares)
}
rmse6
rmst6
plot(iter6,rmse6)


#####################################
# Logistic Regression
rm(list=ls(all=TRUE))
library(ROCR)
library(caTools)
# Importing the context
context = read.csv('OnlineNewsPopularity.csv')
context1 = context[,-c(1,2,38,39)]
summary(context1)
context1=context1[!context1$n_unique_tokens==701,]
summary(context1)
mean(context1$shares)

context1$shares<-ifelse(context1$shares>3395,1,0)
summary(context1)

set.seed(100)
split = sample.split(context1$shares, SplitRatio = 0.70)
training_set = subset(context1, split == TRUE)
test_set = subset(context1, split == FALSE)

training_set[,1:56] = scale(training_set[,1:56])
test_set[,1:56] = scale(test_set[,1:56])

classifier = glm(formula = training_set$shares ~ .,
                 family = binomial,
                 data = training_set)

prob_pred = predict(classifier, type = 'response', newdata = test_set[-57])

y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 57], y_pred > 0.5)
cm
prediction_rate=(cm[1,1]+cm[2,2])/sum(cm)
print(paste("Prediction rate: ",prediction_rate))
print(paste("Error rate: ",1-prediction_rate))

pred<-prediction(prob_pred,test_set[,57])
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf)
area_under_curve<- performance(pred, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

pred
