setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Assignment3")

source("wrappers.R")
library(glmnet)
library(rpart)
library(randomForest)
library(kknn)
library(boot)

ms_train <- read.csv("ms.train.ass3.2019.csv", header = TRUE)
ms_test <- read.csv("ms.test.ass3.2019.csv", header = TRUE)

### Q2.1a ###,

ks<-c(seq(1,25,sep=1))
knn$best.parameters
knn<-train.kknn(intensity~MZ,ms_train,ks=ks,kernel = "optimal")
ytest.hat = fitted( kknn(intensity~MZ ,ms_train , ms_test, kernel = knn$best.parameters$kernel, k = knn$best.parameters$k) )

mean((ms_test$intensity-ytest.hat)^2)

vec<-c()
for (i in ks){
  
  ytest.hat = fitted( kknn(intensity~MZ ,ms_train , ms_test, kernel = knn$best.parameters$kernel, k = i) )
  mse=mean((ms_test$intensity-ytest.hat)^2)
  vec=append(vec,mse)
  
}
plot(y=vec,x=ks,xlab = "k-value", ylab = "Errors")

### Q2.1B ###

library(ggplot2)

knn2<-fitted( kknn(intensity~MZ ,ms_train , ms_test, kernel = knn$best.parameters$kernel, k = 2) )
knn2df<-data.frame(MZ=ms_test$MZ,intensity=knn2)
ggplot(data=ms_train,aes(x=MZ,y=intensity))+geom_point(color='red')+geom_point(data =ms_test,color='blue' )+
  geom_point(data=knn2df,color='yellow')

knn5<-fitted( kknn(intensity~MZ ,ms_train , ms_test, kernel = knn$best.parameters$kernel, k = 5) )
knn5df<-data.frame(MZ=ms_test$MZ,intensity=knn5)
ggplot(data=ms_train,aes(x=MZ,y=intensity))+geom_point(color='red')+geom_point(data =ms_test,color='blue' )+
  geom_point(data=knn5df,color='yellow')

knn10<-fitted( kknn(intensity~MZ ,ms_train , ms_test, kernel = knn$best.parameters$kernel, k = 10) )
knn10df<-data.frame(MZ=ms_test$MZ,intensity=knn10)
ggplot(data=ms_train,aes(x=MZ,y=intensity))+geom_point(color='red')+geom_point(data =ms_test,color='blue' )+
  geom_point(data=knn10df,color='yellow')

knn25<-fitted( kknn(intensity~MZ ,ms_train , ms_test, kernel = knn$best.parameters$kernel, k = 25) )
knn5df<-data.frame(MZ=ms_test$MZ,intensity=knn5)
ggplot(data=ms_train,aes(x=MZ,y=intensity))+geom_point(color='red')+geom_point(data =ms_test,color='blue' )+
  geom_point(data=knn5df,color='yellow')

### Q2.3 ###
var(ms_test$intensity-ytest.hat2)

### Q2.5 ###
cv = learn.tree.cv(intensity~MZ, data = ms_train, nfolds = 10, m = 1000)

prediction <- predict(cv$best.tree, newdata = ms_test)

mean((ms_test$intensity-prediction)^2)


tree<-data.frame(MZ=ms_test$MZ,intensity=prediction)

cv$best.tree$terms[[2]]

ggplot(data=ms_train,aes(x=MZ,y=intensity))+geom_point(color='red')+geom_point(data =ms_test,color='blue' )+
  geom_point(data=tree,color='yellow')

### Q2.7 ###
boot.knn = function(formula, data, indices)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  yhat=fitted( kknn(formula ,ms_train , ms_test, k = 2) )
  # Compute the AUC and return it
  
  return(yhat)
}

booknn<-boot(data=ms_test, statistic=boot.knn, R=100, formula=intensity~MZ)
booknn$t
boot.ci(booknn,conf=0.95,type="bca")

