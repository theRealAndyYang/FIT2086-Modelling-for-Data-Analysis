rm(list=ls())

# Q4.7
pima.train = read.csv("pima.train.csv")
source("my.prediction.stats.R")
pima.test = read.csv("pima.test.csv")

# Full model with no transformations -- for reference
fullmod = glm(DIABETES ~ . , data=pima.train, family=binomial)
summary(fullmod)
# res. deviance of 618.08
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)


# Fit a full model with all interactions, logs of all variables and squares of all variables
# All interactions can be specified using .*.
# there is no quick way of including all the squares or logs
# Resulting model has 53 predictors.
fullmod = glm(DIABETES ~ . + .*. + log(PREG+1) + log(PLAS) + log(BP) + log(SKIN) + log(INS) + log(BMI) + log(PED) + log(AGE) 
                           + I(PREG^2) + I(PLAS^2) + I(BP^2) + I(SKIN^2) + I(INS^2) + I(BMI^2) + I(PED^2) + I(AGE^2) , 
                             data=pima.train, family=binomial)
# lets have a look at how well this model with many parameters does
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)

# Lets see if we can improve things by pruning with BIC
forward.fit.bic = step(fullmod, k = log(nrow(pima.train)), trace=0, direction="both")
summary(forward.fit.bic)
my.pred.stats(predict(forward.fit.bic,pima.test,type="response"),pima.test$DIABETES)

# this model is a lot simpler, and performs better
# its deviance is substantially smaller than the other models but the number of parameters is still smaller
# suggestive that the associations are real



# Q4.8
# Finally, we can use k=3 instead of 2
# This is another commonly used information criteria called the "KIC"
# Lets see if we can improve things by pruning
forward.fit.bic = step(fullmod, k = 3, trace=0, direction="both")
summary(forward.fit.bic)
my.pred.stats(predict(forward.fit.bic,pima.test,type="response"),pima.test$DIABETES)

# this model has the best predictive performance we have seen so far, but has many more parameters
# it has even included some of the interactions
# but our BIC model is a lot easier to understand 