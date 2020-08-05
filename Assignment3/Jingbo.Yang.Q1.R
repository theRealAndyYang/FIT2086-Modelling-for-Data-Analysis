setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Assignment3")
source("wrappers.R")
library(glmnet)
library(rpart)
library(randomForest)
library(kknn)
library(pROC)

library('rpart.plot')

### Q1.1 ###

heart_train <- read.csv("heart.train.ass3.2019.csv", header = TRUE)
summary(heart_train)
cv = learn.tree.cv(HD ~ ., data = heart_train, nfolds = 10, m = 1000)
tree.heart = rpart(HD ~ ., heart_train)
plot(cv$best.tree)
text(cv$best.tree,pretty=12)

tree.heart$variable.importance

### Q1.2 ###
rpart.plot(cv$best.tree,2)


### Q1.3 ###
tree.heart

### Q1.4 ###

### Q1.5 ###


fitglm<-glm(HD~.,data=heart_train,family = binomial)
summary(fitglm)


fit.sw.bic = step(fitglm, k = log(length(heart_train$HD)))

summary(fit.sw.bic)

### Q1.7 ###
my.pred.stats <- function(prob, target, display = TRUE)
{
  rv = list()
  
  classes = levels(target)
  
  # Convert probabilities to best guesses at classes
  pred = factor(prob > 1/2, c(F,T), classes)
  
  # Compute statistics
  T = table(pred,target)
  roc.obj = roc(response=as.numeric(target)-1, as.vector(prob), quiet=TRUE)
  rv$ca   = mean(pred==target)
  rv$sens = T[2,2]/(T[1,2]+T[2,2])
  rv$spec = T[1,1]/(T[1,1]+T[2,1])
  rv$auc  = as.numeric(roc.obj$auc)
  
  # Prob is probability of success, so if the target is not a success, flip the probability
  # to get probability of failure
  prob[target==classes[1]] = 1 - prob[target==classes[1]]
  # Also make sure we never get exactly zero or one for probabilities due to numerical rounding
  prob = (prob+1e-10)/(1+2e-10)
  
  rv$log.loss = -sum(log(prob))
  
  # Display, if requested    
  if (display == TRUE)
  {
    cat("---------------------------------------------------------------------------\n")
    cat("Performance statistics:\n")
    cat("\n")
    cat("Confusion matrix:\n\n")
    print(T)
    cat("\n")
    cat("Classification accuracy =", rv$ca, "\n")
    cat("Sensitivity             =", rv$sens, "\n")
    cat("Specificity             =", rv$spec, "\n")
    cat("Area-under-curve        =", rv$auc, "\n")
    
    cat("Logarithmic loss        =", rv$log.loss, "\n")
    cat("\n")
    
    #plot(roc.obj)
    
    cat("---------------------------------------------------------------------------\n")
  }
  else 
  {
    #
    return(rv)
  }
}

predglm<-predict(fit.sw.bic,type = "response")
predtree<-predict(cv$best.tree)

my.pred.stats(predglm,heart_train$HD)
my.pred.stats(predtree[,2],heart_train$HD)

### Q1.8 ###
heart_test <- read.csv("heart.test.ass3.2019.csv", header = TRUE)


predict(fit.sw.bic,type='response',newdata =heart_test[45,])
predict(cv$best.tree,newdata =heart_test[45,])

library(boot)




### Q1.9 ###
boot.prob = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the AUC and return it
  prob=predict(fit,newdata=heart_test[45,],type = "response")
  return(prob)
}



bs_prob<-boot(heart_test,boot.prob,R=5000,formula=HD ~ CP + EXANG + OLDPEAK + CA + THAL)

boot.ci(bs_prob,conf=0.95,type="bca")



### Q1.10 ###

boot.ca = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the AUC and return it
  target = as.character(fit$terms[[2]])
  rv = my.pred.stats(predict(fit,d,type="response"), d[,target], display=F)
  return(rv$ca)
}

bs = boot(data=heart_train, statistic=boot.ca, R=1000, formula=HD ~ CP + EXANG + OLDPEAK + CA + THAL)
boot.ci(bs,conf=0.95,type="bca")



