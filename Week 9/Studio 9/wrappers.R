# Wrapper for cv.ncvreg
cv.ncvreg.f <- function(formula, data, ...) {
  
  # Make sure the outcome variable is the first column
  mf = model.frame(formula = formula, data = data)
  
  # get the outcome
  y = as.matrix(mf[,1])
  
  # get the predictors and remove the intercept column
  X = model.matrix(formula, data=mf)
  X = X[,-1]   
  
  fit <- cv.ncvreg(X, y, ...)
  fit$formula <- formula
  
  return(fit)
}

# Wrapper for ncvreg
ncvreg.f <- function(formula, data, ...) {
  
  # Make sure the outcome variable is the first column
  mf = model.frame(formula = formula, data = data)
  
  # get the outcome
  y = as.matrix(mf[,1])
  
  # get the predictors and remove the intercept column
  X = model.matrix(formula, data=mf)
  X = X[,-1]   
  
  fit <- ncvreg(X, y, ...)
  fit$formula <- formula
  
  return(fit)
}


# wrapper for predict.ncvreg
predict.ncvreg.f <- function(fit, data, ...) {
  
  # Make sure the outcome variable is the first column
  mf = model.frame(formula = fit$formula, data = data)
  
  # get the predictors and remove the intercept column
  X = model.matrix(fit$formula, data=mf)
  X = X[,-1]   
  
  yhat <- predict(fit, X,  ...)
  
  return(yhat)
}


# wrapper for glmnet
glmnet.f <- function(formula, data, ...)
{
  # Make sure the outcome variable is the first column
  mf = model.frame(formula = formula, data = data)
  t = terms.formula(formula, data=data)
  
  # get the outcome
  y = as.matrix(mf[,1])
  
  # get the predictors and remove the intercept column
  X = model.matrix(t, data=mf)
  X = as.matrix(X[,-1])
  
  fit <- glmnet(X, y, ...)
  fit$formula <- formula
  
  return(fit)
}

# wrapper for cv.glmnet
cv.glmnet.f <- function(formula, data, ...)
{
  # Make sure the outcome variable is the first column
  mf = model.frame(formula = formula, data = data)
  t = terms.formula(formula, data=data)
  
  # get the outcome
  y = as.matrix(mf[,1])
  
  # get the predictors and remove the intercept column
  X = model.matrix(t, data=mf)
  X = X[,-1]   
  
  fit <- cv.glmnet(X, y, ...)
  fit$formula <- formula
  
  return(fit)
}

# wrapper for predict.glmnet
predict.glmnet.f <- function(fit, data, ...) {
  
  # Make sure the outcome variable is the first column
  mf = model.frame(formula = fit$formula, data = data)
  t = terms.formula(fit$formula,data=data)
  
  # get the predictors and remove the intercept column
  X = model.matrix(t, data=mf)
  X = X[,-1]   
  
  yhat <- predict(fit, X,  ...)
  
  return(yhat)
}

# Display only important coefficients
glmnet.tidy.coef <- function(x){
  x <- coef(x)
  df = data.frame(term=rownames(x),
                  estimate=matrix(x)[,1],
                  stringsAsFactors = FALSE)
  df = df[df[,2]!=0,]
  print(df)
}

# Complex formula
my.make.formula <- function(target,df,use.interactions=F,use.logs=F,use.squares=F,use.cubics=F)
{
  # extract variables
  v = names(df)
  v = v[v!=target] 
  nv = length(v)
  
  if (use.interactions)
  {
    f = paste0(target, " ~ . + .*. ")
  }
  else 
  {
    f = paste(target, " ~ . ")
  }
  
  # use logs?
  if (use.logs)
  {
    for (i in 1:nv)  
    {
      if (min(df[,v[i]]) > 0)
      {
        f = paste0(f, "+ log(", v[i], ") ")
      }
    }
  }
  
  # use squares?
  if (use.squares)
  {
    for (i in 1:nv)  
    {
      f = paste0(f, "+ I(", v[i], "^2) ")
    }
  }
  
  # use cubics?
  if (use.cubics)
  {
    for (i in 1:nv)  
    {
      f = paste0(f, "+ I(", v[i], "^3) ")
    }
  }  
  
  return(as.formula(f))
}

learn.tree.cv <- function(formula, data, nfolds, m)
{
  # Compute CV scores
  cv.stats = list()
  tree = rpart(formula=formula,data=data,control=rpart.control(xval=nfolds))
  t = tree$cptable
  
  cv.stats$cp = t[,1]
  cv.stats$cv.err = t[,4]
  
  for (i in 2:m)
  {
    tree = rpart(formula=formula,data=data,control=rpart.control(xval=nfolds))
    t = tree$cptable
    cv.stats$cv.err = cv.stats$cv.err + t[,4]
  }
  cv.stats$cv.err = cv.stats$cv.err/m
  cv.stats$n.leaves = rep(0, length(cv.stats$cp))
  
  # Determine how many splits each CP corresponds to
  for (i in 1:length(cv.stats$cp))
  {
    pruned.tree = prune(tree,cp=cv.stats$cp[i])
    cv.stats$n.leaves[i]=length(unique(pruned.tree$where))
  }
  
  cp = cv.stats$cp[which.min(cv.stats$cv.err)]
  return(list("best.tree"=prune.rpart(tree,cp=cp),"cv.stats"=cv.stats,"best.cp"=cp))
}

plot.tree.cv <- function(cv)
{
  plot(cv$cv.stats$n.leaves, cv$cv.stats$cv.err, xlab="Number of Leaves", ylab="CV Error")
  i = which.min(cv$cv.stats$cv.err)
  points(cv$cv.stats$n.leaves[i], cv$cv.stats$cv.err[i], col="red")
}