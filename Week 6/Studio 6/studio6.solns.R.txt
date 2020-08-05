####################################################################################
#	Script-file:   studio6_skel.R
#	Project:       FIT2086 - Studio 6
#
# Purpose:  	   Solutions for questions in Studio 6
####################################################################################

# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------

rm(list=ls())

# 3.1
df = read.csv("toydata.csv")
plot(df$X, df$y, xlab="X", ylab="y")

# 3.2
lm(y~X,data=df)
# when x = 0, the predicted value of y is -0.1883
# for every unit increase in x, the predicted value of y increases by 1.1035

# 3.3
fit = lm(y~X,data=df)
summary(fit)
# p-value is 0.0019, which is strong evidence against the null that beta_1 = 0;
# so very likely that X is associated with y 

# 3.4
yhat = fit$coefficients[[1]] + fit$coefficients[[2]] * df$X
yhat

yhat = predict(fit, df)
yhat
lines(df$X, yhat, lwd=2)

# the fit is decent but the last data-point seems to be fitted quite poorly by
# the line

# 3.5
fit2 = lm(y ~ X, data=df, subset=1:9)
summary(fit2)

# The estimate for beta_1 changes from 1.1035 to 1.4934 which is a change of 35%. This is quite considerable
# The p-value drops to 4 x 10^-6 which is much smaller than before (1.9 * 10^-3)
# The R^2 increases from 0.7193 to 0.9588. This is a very large increase in goodness-of-fit
# It certainly seems that removing this datapoint means the simple linear model fits the remaining
# data points a lot better.

# 3.6
yhat2 = predict(fit2, df)
lines(df$X, yhat2, lwd=2, col="red")
# We now see that the red line passes much more closely through the first 9 data points, but is even worse
# at predicting the 10th (as we did not include it in our fit). We would probably do well to treat
# this data point as seperate from our model.


# 3.7
# Confidence interval for our fitted model
plot(df$X, df$y, xlab="X", ylab="y")
yhat2 = predict(fit2, df, interval="confidence")
lines(df$X, yhat2[,"fit"], col="red")
lines(df$X, yhat2[,"lwr"], col="black")
lines(df$X, yhat2[,"upr"], col="black")

# the upper and lower lines give a range of the plausible values of our line if we were to draw a new sample
# and estimate it again from data. The red line in the middle is just our "best guess" at the line found
# by using our least-squares estimates


# ----------------------------------------------------------------------------------
# Question 4
# ----------------------------------------------------------------------------------

# 4.1
rm(list=ls())
wine = read.csv("wine_train.csv")

hist(wine$fixed.acidity)
hist(wine$volatile.acidity)
hist(wine$citric.acid)
# etc ..

# all the predictors are continuous variables and seem to be (normal-ish distributed)

hist(wine$quality)
# the target (quality) is a discrete variable that takes on a finite number (11, 0 through to 10) of values
# not exactly normally distributed but if the variable is discrete and the number of values is larger than 
# 5 or 6 we can still often use least-squares with decent success

# 4.2
fit = lm(quality ~., wine)
summary(fit)

# volatile.acidity, residual.sugar and free.sulfur.dioxide seem to be borderline (p < 0.1) associated
# and are our best guesses using this method of which variables might be associated with quality.
# remember, in this case the p-value is the evidence against the null hypothesis that the coefficient has a value of 
# zero at the population level (that is, it is unassociated with the target)
#
# p-values of 0.05-0.1 mean that the chance of seeing an association just by chance, if there was no association at the population level,
# is around %5-10% -- which is not super likely, but also not particuarly unlikely either -- hence these are "borderline"

# 4.3
plot(predict(fit,wine), wine$quality)

# Looking at this plot we can interpret it in the following way. It plots the predicted quality of a wine on the x-axis 
# against the actual quality of that particular wine on the y-axis, for all the wines in our training data sample. So for 
# example, from our plot, we can see that when our model predicts quality to be around 6, the actual quality value are
# concentrated around 5-6, and when our model predicts the quality to be around 7, the actual quality scores are larger on
# average, and approximately around 7. Of course the predictions are not perfect. If they were, we would expect our predicted 
# value to exactly equal our actual values of quality in the data, which would result in a diagonal line. The more 
# ``diagonal'' the line the better the fit. In this example there is clearly some concordance between the predictions 
# and the actual values and the overall trend is somewhat diagonal -- so our model is doing a decent job of predicting 
# wine quality in our data.

# 4.4
wine.test = read.csv("wine_test.csv")
yhat_full = predict(fit, wine.test)
mean((yhat_full - wine.test$quality)^2)

# the error tells us how good our model is at predicting the quality of wines from NEW, unseen data
# the smaller the error the better the model fit; an error of zero being a perfect prediction.
# The value in this case is 0.619 which is the average squared error between our predictions and the new data
# Square-rooting this gives an error of 0.787, which can be (roughly) interpreted this way: on average, for future data
# our predicted quality for a wine given the chemical measurements will be different by 0.787 units in either direction
# from  the actual quality of the wine. This is not exactly right, but is a close enough interpretation
# to be useful.

# 4.5
fit_aic = step(fit)
summary(fit_aic)

# we see that the stepwise procedure (which tries to remove "unimportant" variables by minimising the Akaike information criterion score)
# has removed 6 of our 11 predictors, leaving us with volatile.acidity, residual.sugar, free.sulfur.dioxide (which the p-values 
# suggested were potentially important) as well as density and alcohol, which the p-values did not suggest were important
# (both were > 0.15)
#
# the p-values of the remaining variables have all gone down, particularly those p-values for alcohol and density.

# How well does it predict?
yhat_full_aic = predict(fit_aic, wine.test)
mean((yhat_full_aic - wine.test$quality)^2)

# the MSPE is 0.611 which is smaller than 0.619 obtained using the full model.
# so it actually performs better than using all the variables. This suggests that at least some of the 6 variables we have removed
# are not actually associated with quality, and any predictions made using them would just be adding "noise" to our predictions
# we not that the p-values for the retained variable are now all smaller

# 4.6
summary(fit)
summary(fit_aic)

# the R^2 for the full model is 0.4, while for the smaller model selected by stepwise it is 0.389. As has been mentioned in 
# the lectures, the more variables you include in a model, the better the fit to the data you are training the model on will be --
# and therefore, the better the R^2. However, if some of those predictors are not really associated with the target, or are so
# weakly associated that it is difficult to properly estimate their coefficients, we can improve prediction error on NEW data
# by removing them -- as has been done by our stepwise procedure.

# 4.7
fit_aic$coefficients
# the equation is:
# E[quality] = 127.56 - 1.347*volatile.acidity + 0.0857*residual.sugar + 0.0115*free.sulfur.dioxide - 126.1*density + 0.298*alcohol
#
# As alcohol increases, the predicted quality increases. 
# For every unit increase in alcohol content, the quality score goes up by approx. 0.3
#
# As volatile.acidity increases, the predicted quality decreases.
# For every unit increase in volatile.acidity, the quality score decreases by approx 1.34
#
# A manufacturer could use this model to try and determine which characteristics of the wine to reduce/increase to increase
# perceived taste, and therefore, increase sales

# 4.8
# to use BIC we set the "k" parameter to log(number of samples we trained on), i.e.,
fit_bic = step(fit, k = log(length(wine$quality)))
summary(fit_bic)

# the BIC method is more conservative than the AIC method above and has only selected 3 predictors:
# residual.sugar, free.sulfur.dioxide and alcohol.

yhat_full_bic = predict(fit_bic, wine.test)
mean((yhat_full_bic - wine.test$quality)^2)

# the prediction error on NEW data is actually worse in this case. It seems the two predictors BIC omitted but AIC (Q4.5)
# retained (volatile.acidity and density) are actually important for accurately predicting quality, and leaving them
# out has hurt our predictions. 
#
# This does not mean BIC selected models are always worse than AIC selected models -- it just means that it can be
# actually quite tricky to select a good model using just the data. This subject gives you some tools to do so, but
# a large part of data modelling is an art.

# 4.9
fit_wine.test = lm(quality ~ ., wine.test)
summary(fit_wine.test)

# from the results of fitting on a LOT of data (4798 observations, as opposed to just 100 in our original training dataset above)
# we see that all but three of the predictors are found to have very small p-values (mostly in the order of 10^-10 or smaller!)
# The chance of seeing associations this strong just by chance are then absolutely tiny (1 in trillions). Three of the variables
# have much larger p-values and are probably not associated (citric.acid, chlorides, total.sulfur.dioxide).
#
# interestingly, if we look at the model selected by our stepwise procedure in Q4.5, we see that those three variables
# are among the 6 predictors that were dropped from our model -- which potentially explains why our smaller model 
# predicted better onto this new data than the full model using all the predictors did. The associations betwen these 
# three variables and the target that we estimated in the full model were likely to be incorrect and adversely effect
# our predictions.
