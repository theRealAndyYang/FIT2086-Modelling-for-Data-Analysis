rm(list=ls())
library("mclust")

# 2.1
data(diabetes)

# 2.2
class = diabetes$class
table(class)

X = diabetes[,-1]

clp <- clPairs(X, class, lower.panel = NULL)
clPairsLegend(0.1, 0.3, class = clp$class, col = clp$col, pch = clp$pch)

# 2.3
mod1 = Mclust(X, modelNames = "VVI")
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")

table(class, mod1$classification)

# 2.4
BIC <- mclustBIC(X)
BIC
summary(BIC)

# 2.5
plot(BIC)

# 2.6
mod1 = Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)

# 2.7
# How well did we classify?
table(class, mod1$classification)

# Plot classifications
plot(mod1, what = "classification")

# 3.1
source("my.k.means.sol.R")
X = read.csv("kmeans.test.csv")

# k-means ...
rv = my.k.means(X,3)
plot(X[,1],X[,2],col=rv$w.cluster)
