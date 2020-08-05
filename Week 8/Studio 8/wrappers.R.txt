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
  X = X[,-1]   
  
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
