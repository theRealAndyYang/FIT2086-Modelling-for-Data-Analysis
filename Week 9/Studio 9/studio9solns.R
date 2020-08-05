####################################################################################
#	Script-file:   studio9.solns.R
#	Project:       FIT2086 - Studio 9
#
# Purpose:  	   Solutions for questions in Studio 9
####################################################################################


# ----------------------------------------------------------------------------------
# Question 2
# ----------------------------------------------------------------------------------

rm(list=ls())


# ------------------------------------------------------------------------------------------------------
# 2.1
setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Week 9/Studio 9")
source("my.prediction.stats.R")
source("wrappers.R")
library(glmnet)
library(rpart)
library(randomForest)
library(kknn)

diabetes.train = read.csv("diabetes.train.csv")
diabetes.test = read.csv("diabetes.test.csv")
summary(diabetes.train)

# We can see that all variables except sex are numeric

# 2.2
tree.diabetes = rpart(Y ~ ., diabetes.train)

# 2.3
tree.diabetes

# To interpret this output, note that each line represents a node in the tree.
#
# The first piece of information is the "node number", which labels each node uniquely
#
# The second piece of information is the condition that must be satisfied to reach this node 
# The initial node in the tree is the "root" and contains all 354 individuals in the data.
#
# Node 2, for example, is reached by having a BMI < 27.75, and contains 238
# individuals. Node 4 is reached by having first a BMI < 27.75, and then an S5 < 4.61005,
# and 148 individuals satisfy these two conditions, and so on.
#
# The fourth number is a measure of "goodness-of-fit", and is not that important for our purposes
#
# The fifth number is the predicted value of diabetes progression (Y) for that node.
# Every node has a prediction associated with it. For example, the root node prediction of 148.3 is
# equal to mean(diabetes.train$Y) as it is the mean for all individuals, while the
# prediction for node 2) is mean(diabetes.train$Y[diabetes.train$BMI<27.75]) = 120.1, and so on.
#
# For classification problems, the predicted value will be the most likely class. 
# This will then be followed by the probabilities of being in each of the possible target classes.
#
# The most important nodes for prediction are the ones with "*" next to them -- these are the
# leaf (terminal) nodes in the tree. From the output we can see there are 12 leaf nodes in 
# this tree.
#
# There are 14 leaf (terminal) nodes in this tree.
#
# The variables used are: BMI, BP, S1, S2, S3, S5, S6

# 2.4
plot(tree.diabetes)
text(tree.diabetes, digits=3)

#
# Note that a tree may split on the same numeric variable more than once.
# This allows the tree to have splits like: 
#
# (-infinity < S5 < 4.167), (4.167 < S5 < 4.61), (4.61 < S5 < 4.883), (4.883 < S5 < infinity)
# 
# as can be seen in the left sub-tree of our decision tree.
# Remember that any tree can be represented by a binary tree, so we do not 
# lose any generality by only splitting numeric predictors in a binary fashion
# as long as we allow a numeric predictor to be split more than once.
#
# For categorical predictors, several categories may lead to the same branch when splitting --
# that is, the tree does not necessarily split a K-category variable into K leaves; several
# different category values may lead to the same leaf.

# 2.4a
# If BMI = 28, BP = 96 and S6 = 110, we take the right-hand subtree at the first split,
# then the left-hand branch, then the right-hand branch, and arrive at the leaf with 
# a predicted diabetes progression score of 244
#
# 2.4b
# If BMI = 20.1, S5 = 4.7 and S3 = 38, we take the left-hand subtree at the first split,
# then the right-hand branch, then the left-hand branch, then finally the right-hand branch
# to arrive at the leaf node with a prediction score of 161
# 
# 2.4c
# The highest predicted diabetes progression score is 262, which is in the right-most leaf
# in the tree. To get to this leaf, we note that we need a BMI > 31.35 and a BP > 101.5
# So having a very high BMI is (unsurprisingly) a strong predictor of poor diabetes progression.
#
# You can find the same information by traversing the tree displayed in the R console in Q2.3
# Try this yourself to make sure you understand the ideas.

# 2.5
tree.diabetes$variable.importance

tree.diabetes$variable.importance / max(tree.diabetes$variable.importance)

# BMI, S5 and BP are the three most important. BMI seems by far the most important as it
# has around 1.5 timesthe importance score of S5.

# 2.6
sqrt(mean((predict(tree.diabetes, diabetes.test) - diabetes.test$Y)^2))
#
# RMSE = root-mean-squared error, i.e., the square root of the average squared prediction
# error made by our model on future data.
#
# The RMSE can be interpreted as follows: if we were to randomly draw a new individual from the
# population we built our tree on, predict their diabetes progression score using the particular
# values of their predictors, calculate the difference between the predicted score and their actual score
# and square, then square-rooted this value, the result would on average be (roughly) equal to the 
# RMSE score calculated above. 
#
# So it is the average squared error we would expect to obtain predicting on new data using our tree.
# The smaller this score, the better our predictions.

# 2.7
cv = learn.tree.cv(Y~.,data=diabetes.train,nfolds=10,m=1000)
plot.tree.cv(cv)

#
# The CV plot shows that the best tree size is around 7. This is the number
# of leaf nodes the tree should be grown to have. The initial tree we learned
# used a simple heuristic to decide when to stop growing, and can overfit.
# CV lets us reduce the number of leaves by trying to minimise prediction error on future data.
#
# As CV is a random process, sometimes it can produce a slightly different
# estimate of the number of leaf nodes to use, but they will generally be around 
# 7. Taking m=1000 reduces the chance of a different size tree than 7 having the
# best CV score, but the CV process therefore takes longer as we are doing more CV iterations
# In general you want to make m as large as reasonably possible to reduce randomness
#
# The curve shows that for 7,8,9 leaf nodes the CV errors are very similar
# and it would be difficult to decide exactly which size tree is best.

prune.rpart(tree=tree.diabetes, cp = cv$best.cp)
# This prunes the tree using the optimal pruning parameter "best.cp" (look at wrappers.R, this
# is what is done in learn.tree.cv() to get our pruned tree).
#
# *Note: We never need to prune by hand -- the best tree is returned in cv$best.tree*

# 2.8
cv$best.tree
#
# The CV pruned tree has removed three of the predictors (S1, S2 and S3)
# and has less leaf nodes (7)

sqrt(mean((predict(cv$best.tree, diabetes.test) - diabetes.test$Y)^2))
#
# The RMSE is actually a *little* worse than the tree before, but 
# the tree is a lot simpler. Simplfying a tree using cross-validation will not be
# guaranteed to always improve prediction scores, but it will generally do
# at most only a little worse than the full tree, and can do a lot better
# if the full tree is overfitting. Additionally, it will always produce a 
# more interpretable model as the tree will be simpler.

# We can look at the new tree in the console
cv$best.tree

# We can also plot the tree as before
plot(cv$best.tree)
text(cv$best.tree)

# The characteristics that lead to the worst diabetes progression are now having
# a BMI > 27.75 (as above), but a BP < 101.5 and an S6 > 101.5
#
# This shows the potential instability of tree models. 
# In our original tree we needed a BP > 101.5 to get to the leaf with the highest
# diabetes progression -- now we need a BP < 101.5. SO simplifying the tree
# has modified its behaviour somewhat.
#
# However, note that if we have BMI > 27.75 and BP > 101.5, we still predict a
# diabetes progression of 242.9 which is only slightly lower than 243.8.
# So the tree can have similar behaviour but a different structure.

# 2.9
lasso.fit = cv.glmnet.f(Y ~ ., data=diabetes.train)
glmnet.tidy.coef(lasso.fit)
sqrt(mean((predict.glmnet.f(lasso.fit,diabetes.test)-diabetes.test$Y)^2))

# In this case the linear model selects (in general -- if not rerun it a few times)
# the predictors BMI, BP, S3, S5, which are predictors deemed important
# in the trees above. The linear model outperforms our decision tree
# in prediction error, which suggests that the underlying relationship between
# diabetes progression and these variables is at least somewhat linear.
#
# It is always important to compare against a standard linear model fit using
# something like lasso, as linear/logistic models serve as an excellent 
# benchmark method.


# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------

# 3.1
rf.diabetes = randomForest(Y ~ ., data=diabetes.train)

# 3.2
rf.diabetes

# We see from this that our forest contains 500 trees
# The number of variables selected at random to try splitting on
# at each step of the forwards tree growing algorithm is 3 (in this case,
# it is heuristically set by the random forest software depending on the number of predictors
# available).
#
# The mean of squared residuals is the average squared error of our
# random forest when predicting on the TRAINING data.
#
# The % Var explained is essentially equal to 100 times the "R^2" value of the
# random forest (remember, R^2 ranges from 0 (no ability to explain the data) to 
# 1 (perfect explanation of the data))
#
# So in this case, our random forest has an (approximate) R^2 of about 0.435 (43.5ish/100)

# 3.3
sqrt(mean((predict(rf.diabetes, diabetes.test) - diabetes.test$Y)^2))

# The random forest does substantially better than the single "best" decision tree,
# and a little better than the linear model. Suggests that there may be some nonlinear relationship
# between diabetes progression and the predictors
#
# However, it gains this small increase in predictive performance at the expense
# of being a model you cannot understand. Obviously no one can make sense of 500 trees while
# the linear model is very easy to interpret. From the linear model 
# it is straightforward to see how BMI and BP, for example, affect our predicted diabetes progression


# 3.4
rf.diabetes = randomForest(Y ~ ., data=diabetes.train, importance=TRUE, ntree=5000)
sqrt(mean((predict(rf.diabetes, diabetes.test) - diabetes.test$Y)^2))

# Takes longer to run, but in this case (small number of predictors, p = 10)
# it makes very little difference to the predictions.
# If the number of predictors was larger it may have a bigger impact

# 3.5
round( importance( rf.diabetes ), 2)
#
# The %IncMSE measure is an indication of how much the random forest software believes
# our mean-squared prediction error on new data would increase if we omitted the variable.
#
# For example, if we did not allow the random forest to use the BMI variable, we would expect
# a 99ish% increase (doubling) of our mean-squared error.
#
# Using this, it seems that BMI, BP and S5 are the stand-out important variables as omitting
# them would lead to large increases in prediction error. This matches the top three
# most important variables as suggested by a single tree using rpart.
#
# The IncNodePurity score can be interpreted in a similar way (larger = more important variable)
# It indicates how much purer the leaf nodes in the tree will be if we include a variable, so 
# variables with a high score (BMI, BP, S5, S6) seem important by this measure.


# ----------------------------------------------------------------------------------
# Question 4
# ----------------------------------------------------------------------------------

# 4.1
ytest.hat = fitted( kknn(Y ~ ., diabetes.train, diabetes.test) )
sqrt(mean((ytest.hat - diabetes.test$Y)^2))

# With default settings for the number of neighbours to use, the kNN method does a bit better 
# than the decision tree, but worse than the linear model and the random forest.
#
# Note that we do not learn a model -- we just use the training data (diabetes.train)
# to predict the diabetes progression for the people in diabetes.test
#

# 4.2
kernels = c("rectangular","triangular","epanechnikov","gaussian","rank","optimal")
knn = train.kknn(Y ~ ., data = diabetes.train, kmax=25, kernel=kernels)
ytest.hat = fitted( kknn(Y ~ ., diabetes.train, diabetes.test,
                         kernel = knn$best.parameters$kernel, k = knn$best.parameters$k) )

knn$best.parameters$kernel
knn$best.parameters$k

sqrt(mean((ytest.hat - diabetes.test$Y)^2))

# This code tries 5 different types of kernels, and neighbourhood sizes k=1 through to 25
# and chooses the combination that minimises the cross-validation error
#
# The "kernel" is a function that decides how the target values of the k closest 
# points in the training data are combined. "Rectangular" kernel is equivalent to just
# using the average of all the target values of the k nearest neighbours, while
# the others weight the target values inversely proportionally to their distance from 
# the point we are trying to predict.
#
# The resulting model using knn$best.parameters$kernel ("gaussian", in this case) and 
# knn$best.parameters$k (24, in this case) is about as good as the linear model but 
# a little worse than the random forest.
