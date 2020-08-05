####################################################################################
#	Script-file:   studio8.solns.R
#	Project:       FIT2086 - Studio 8
#
# Purpose:  	   Solutions for questions in Studio 8
####################################################################################


# ----------------------------------------------------------------------------------
# Question 2
# ----------------------------------------------------------------------------------

# 2.2
n = 1
sigma2 = 1

mu = seq(from=-5, to=5, by=0.001)
plot(c(-5,5), c(1,1)*sigma2/n, type="l", xlab="Population mean (mu)", ylab="MSE", ylim=c(0,5))

c = 0.1
lines(mu, (c/(n+c))^2*mu^2 + n*sigma2/(n+c)^2, col = "red")

c = 1
lines(mu, (c/(n+c))^2*mu^2 + n*sigma2/(n+c)^2, col = "green")

c = 5
lines(mu, (c/(n+c))^2*mu^2 + n*sigma2/(n+c)^2, col = "blue")

# Clearly as c increases, the MSE near mu = 0 goes down, but
# the range for which our shrinkage estimator is better than the sample mean
# becomes smaller and smaller, and the estimator does worse and worse outside
# of the small area in which it is better. Clearly, careful
# selection of c is required.


# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------

rm(list=ls())


# ------------------------------------------------------------------------------------------------------
# 3.1
source("my.prediction.stats.R")

gene.train = read.csv("gene.train.csv", header=T)
gene.test = read.csv("gene.test.csv", header=T)
fullmod=glm(Disease ~ ., data=gene.train, family=binomial)
my.pred.stats(predict(fullmod, gene.test, type="response"), gene.test$Disease)

# As we discovered last week, the classification accuracy of this model
# is poor (only 53% of people correctly classified as diseased/undiseased)
# This is not much better than a random guess and is due to heavy overfitting
# (p=100 predictors fitted to n=200 samples)


# ------------------------------------------------------------------------------------------------------
# 3.2
coefficients(summary(fullmod))
pvalues = coefficients(summary(fullmod))[,4]
pvalues < 0.05
sum(pvalues < 0.05)

# We see that there are 12 variables that pass the "0.05" threshold


# ------------------------------------------------------------------------------------------------------
# 3.3
# We have p = 100, alpha = 0.05
# So would expect to see 100 * 0.05 = 5 by chance.
#
# 12 is bigger than 5, but not dramatically so -- the above expected value
# is just an average number of variables we would expect to see pass just by chance
# and may change from sample to sample. 
#
# However, we know from last week that a good model for this data has only two predictors
# so we see that if we blindly included every predictor with a p-value < 0.05 we would very likely overfit


# ------------------------------------------------------------------------------------------------------
# 3.4
sum(pvalues < 0.05/100)

# The Bonferroni procedure is very conservative -- in this case, none of our predictors have
# a p-value small enough to pass the Bonferroni threshold of 0.05/100 = 5*10^-4.
# The Bonferroni gives us confidence that anything that does pass the threshold is likely
# a real association at the population level, but it is very conservative and will often discover
# nothing as the evidence required to overcome the threshold is too high.
#
# The problem is worse when we fit many predictors together as the strength of association
# of the important predictors is often "diluted" out by including lots of noisy predictors, and the
# effect of that is that even the p-values of important variables can get quite a bit larger than they
# would be if you didn't include the noisy variables


# ------------------------------------------------------------------------------------------------------
# 3.5
fit.ric = step(fullmod, direction="both", k=2*log(ncol(gene.train)-1))
summary(fit.ric)
my.pred.stats(predict(fit.ric, gene.test, type="response"), gene.test$Disease)
#
# The RIC method penalises the likelihood by the number of parameters in the model, times 
# log(the total number of predictors we are trying). In this way it is similar in concept
# to the Bonferroni approach, but is less conservative in general. It has found only a single
# important predictor: SNP56, which was one of the two SNPs we found in Studio 7 using stepwise
# selection and BIC. The model does a good job of predicting onto the test data.
#
# Our final model says:
#
# log-odds(disease) = -0.2231 + 1.2528 SNP56Y
#
# so that having a mutation at SNP56 increases our log-odds of having the disease by 1.2528, or
# equivalently, increases our odds by a FACTOR of exp(1.2528) = 3.5.
#
# The model says that if we do not have a mutation at SNP56, our odds of having disease are exp(-0.2231) = 0.8
# If we have a mutation at SNP56, our odds of having disease are exp(-0.2231 + 1.2528) = 2.8
# (note that 0.8 * 3.5 = 2.8)
#
# So our odds of being diseased are 3.5 times higher if we are unlucky enough to have a genetic mutation
# at SNP56, compared to someone who does not have a genetic mutation at SNP56



# ----------------------------------------------------------------------------------
# Question 4
# ----------------------------------------------------------------------------------

rm(list=ls())
source("wrappers.R")
source("my.prediction.stats.R")
library(glmnet)

pima.train = read.csv("pima.train.csv")
pima.test = read.csv("pima.test.csv")


# ------------------------------------------------------------------------------------------------------
# 4.1
lasso.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial", lambda = 0)
my.pred.stats(predict.glmnet.f(lasso.fit, pima.test, type="response"), pima.test$DIABETES)

coefficients(lasso.fit)

# Gives essentially the same model as using glm() -- this is because for lambda = 0
# we have no penalisation.


# ------------------------------------------------------------------------------------------------------
# 4.2
lasso.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial", lambda = 0.02)
coefficients(lasso.fit)
sum(coefficients(lasso.fit) != 0)

lasso.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial", lambda = 0.03)
coefficients(lasso.fit)
sum(coefficients(lasso.fit) != 0)

lasso.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial", lambda = 0.05)
coefficients(lasso.fit)
sum(coefficients(lasso.fit) != 0)

# As lambda increases, the degree of penalisation in our lasso equation (see Lecture 8) increases,
# and we can see that the magnitude of the coefficients gets smaller and smaller (are shrunken towards zero).
# Remembering that the larger the coefficient, the stronger the association and the 
# more the coefficient "fits" the data, we see that increasing the penalisation reduces 
# the fit of the model to the data -- and therefore reduces the complexity.
#
# Note, in particular, that as lambda increases, some of the predictors are actually shrunken so much towards 
# zero that they are removed from the model. For lambda = 0.02, INS is removed from the model; for lambda = 0.03
# BP and INS are removed, and for lambda = 0.05 then BP, INS, SKIN and PED are all removed.


# ------------------------------------------------------------------------------------------------------
# 4.3
lasso.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial")
plot(lasso.fit, "lambda", label=T)

# The plot shows the "coefficient path" as lambda changes. 
# By tracing along the lines we can see how the coefficients change for the different values of lambda
# Take the black line for example -- this is PED (variable 7 in our dataset) and has a value of approximately 0.8 when lambda = 0.
# As lambda increases, it progressively shrinks towards zero, and is eventually removed
# at around log(lambda) = -2.8. We note that after it is removed, three variables still remain in the model
# and are eventually shrunk to zero as the log(lambda) approaches -1.


# ------------------------------------------------------------------------------------------------------
# 4.4
lasso.fit = cv.glmnet.f(DIABETES ~ ., pima.train, family="binomial")
plot(lasso.fit)
min(lasso.fit$cvm)

# This plot shows how well glmnet thinks models estimated by the lasso, from our data, will predict 
# onto future data for different choices of (log) lambda. For example, we see that if log(lambda) = -2,
# then lambda = exp(-2) = 0.135. If we run glmnet.f() with lambda = 0.135 we will see that the lasso
# removes all variables except for PLAS -- i.e. it is a very simple model. However, we also see
# from the plot that the prediction error is estimated to be very poor. A value of log(lambda) near -5
# appears to offer much better potential prediction on future data.


# ------------------------------------------------------------------------------------------------------
# 4.5
coefficients(lasso.fit)
my.pred.stats(predict.glmnet.f(lasso.fit, pima.test, type="response"), pima.test$DIABETES)

# We see that the lasso does about as well as the full-model without model selection,
# and removes 2 of the 8 predictors. Note that because the cross-validation method glmnet uses
# is random, running the cv.glmnet.f() twice will give slightly different results, though in general
# they will be very similar.


# ------------------------------------------------------------------------------------------------------
# 4.6
fit.bic = step(glm(DIABETES ~ .,pima.train,family="binomial"),k=log(668),direction="both")
my.pred.stats(predict(fit.bic, pima.test, type="response"), pima.test$DIABETES)
summary(fit.bic)
coefficients(lasso.fit)

# BIC does about the same as the lasso model, but in this case has removed 4 of the predictors.
# BIC removed BP, SKIN, INS and AGE, and the lasso removed BP and INS.


# ------------------------------------------------------------------------------------------------------
# 4.7
ridge.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial", lambda = 0.05, alpha=0)
coefficients(ridge.fit)

ridge.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial", lambda = 0.15, alpha=0)
coefficients(ridge.fit)

ridge.fit = glmnet.f(DIABETES ~ ., pima.train, family="binomial", lambda = 0.5, alpha=0)
coefficients(ridge.fit)

# again, we notice that as in lasso, as lambda increases the amount of penalisation increases,
# and the coefficients are all shrunken more and more towards zero. However, unlike
# in the case of the lasso, none of the coefficients are ever set to zero. This is a weakness
# of the ridge method.


# ------------------------------------------------------------------------------------------------------
# 4.8
ridge.fit = cv.glmnet.f(DIABETES ~ ., pima.train, family="binomial", alpha=0)
plot(ridge.fit)

# the CV plot suggests that we should use a very small value of lambda for ridge regression, 
# i.e., no real shrinkage should be done.

coefficients(ridge.fit)
my.pred.stats(predict.glmnet.f(ridge.fit, pima.test, type="response"), pima.test$DIABETES)

# we see the fitted model does about as well as lasso, BIC and the full model (without model selection)
# in this case. This is not surprising as we have n=668 observations and only p=8 predictors
# so most methods will perform about the same.


# ------------------------------------------------------------------------------------------------------
# 4.9
# Fit a full model using glm()
fullmod = glm(DIABETES ~ . , data=pima.train, family=binomial)
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)

# Make a model including all interactions, all logs, all squares and all cubics
# we now have p = 59 total predictors
f = my.make.formula("DIABETES", pima.train, use.interactions=T, use.logs=T, use.squares=T, use.cubics=T)
fullmod = glm(f, data=pima.train, family=binomial)
(coefficients(fullmod))

# lets have a look at how well this model with 59 parameters does
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)
# This model actually does a bit better than the model using only the 8 original untransformed predictors
# despite fitting 59 predictors in total. This suggests that some of these transformations 
# are very important (see Studio 7 for more discussion)

# Now lets prune the model using BIC (and time stepwise)
ptm <- proc.time()
forward.fit.bic = step(fullmod, direction="both", k=log(668), trace=0)
proc.time() - ptm
# Takes ~ 36.8s on my machine (will be different on yours ...)
# stepwise regression can be very slow if the number of potential predictors 
# becomes large, as it must try and fit many different models over and over again.
# The resulting model is quite simple (6 predictors -- see Studio 7 for more discussion of this model)

summary(forward.fit.bic)
my.pred.stats(predict(forward.fit.bic,pima.test,type="response"),pima.test$DIABETES)

# Fit the model using lasso + CV
ptm <- proc.time()
lasso.fit = cv.glmnet.f(f, data=pima.train, family="binomial", nfolds=25)
proc.time() - ptm
# takes ~13.5s on my machine; substantially quicker than stepwise when the number of predictors is large

my.pred.stats(predict.glmnet.f(lasso.fit, pima.test, type="response"), pima.test$DIABETES)
glmnet.tidy.coef(lasso.fit)
# Model contains 6 - 10 predictors (depending on random ness, re-run and you will potentially get a
# slightly different model), with some/most of them different from BIC
# However, in this case BIC predicts a bit better than lasso
# Lasso will not always "win" -- no method can -- and when the sample size is large 
# most methods will do similar in performance. 


# ------------------------------------------------------------------------------------------------------
# 4.10
# (a) Swap training and testing data around
pima.train = read.csv("pima.test.csv")
pima.test = read.csv("pima.train.csv")

# Just fit the original untransformed predictors, and see how well that does
fullmod = glm(DIABETES ~ . , data=pima.train, family=binomial)
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)
# CA = 0.74, AUC = 0.76

# (b) make a model include all interactions, logs, squares and cubics
# Now we have only n=100 samples to fit to our 59 predictors
f = my.make.formula("DIABETES", pima.train, use.interactions=T, use.logs=T, use.squares=T, use.cubics=T)
fullmod = glm(f, data=pima.train, family=binomial)
length(fullmod$coefficients)  # tells you how many predictors exist, after transformation
my.pred.stats(predict(fullmod,pima.test,type="response"),pima.test$DIABETES)
# CA = 0.62, AUC = 0.61 -- lots of overfitting going on as n is now small compared to p
# The red warnings when you ran glm() are suggestive that the model is heavily overfitted
# prediction performance is much worse than the model with just the 8 original predictors
# as with only n=100 samples, we have learned a lot of noise


# ------------------------------------------------------------------------------------------------------
# 4.11
# Compare to the model fitted by stepwise + BIC
step.fit.bic = step(fullmod, direction="both", k=log(nrow(pima.train)), trace=0)
summary(step.fit.bic)
my.pred.stats(predict(step.fit.bic,pima.test,type="response"), pima.test$DIABETES)
# CA = 0.67, AUC = 0.62 -- there is still lots of overfitting going on
# After pruning, the BIC model still has retained 19 predictors.
# Compare to the case when we had n=668 samples -- BIC kept only 6 predictors
# It appears that it is overfitting. This is a potential problem with stepwise regression
# using these basic types of information criteria. They have been designed for
# the "large n, small p" situation, and when n is not large relative to p, they can 
# start to perform badly


# ------------------------------------------------------------------------------------------------------
# 4.12
# Fit use lasso + CV
lasso.fit = cv.glmnet.f(f, data=pima.train, family="binomial", nfolds=25)
plot(lasso.fit)
glmnet.tidy.coef(lasso.fit)
my.pred.stats(predict.glmnet.f(lasso.fit, pima.test, type="response"), pima.test$DIABETES)
# CA = 0.77, AUC = 0.82 
# The lasso only retains 5 (or 6, can vary from run to run due to randomness in the CV procedure) predictors
# It now does better than the basic model including only the 8 untransformed predictors, 
# and without any interactions/logs/squares etc.
# Compare to the case above when we had n=668 samples, lasso retained 6 predictors
# It has selected a very similar complexity model even when n = 100.
#
# In comparison, BIC has broken down and overfitted.
#
# In general, the lasso behaves gracefully as the sample size gets smaller while the stepwise
# methods can break down quite badly. This is a strength of the lasso.
#

# Fit using ridge + CV
ridge.fit = cv.glmnet.f(f, data=pima.train, family="binomial", alpha=0)
my.pred.stats(predict.glmnet.f(ridge.fit, pima.test, type="response"), pima.test$DIABETES)
#
# Ridge does about as well as model not including any extra predictors but worse than Lasso
# It is clear that removing predictors, which the lasso can do but ridge cannot,
# is required to improve performance in this dataset.

# We can use the minimum cross-validation scores from lasso and ridge to see which 
# one glmnet thinks will predict better on future data
min(ridge.fit$cvm)
min(lasso.fit$cvm)

# glmnet says we should prefer the lasso model in this case, as it has estimated
# that the lasso model should have a smaller prediction error on new, unseen data
# -- which in this case, it does; the lasso model has predicted onto future data more effectively.