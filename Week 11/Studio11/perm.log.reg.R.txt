perm.log.reg = function(formula, data, R)
{
  rv = list()
  
  n = nrow(data)
  
  # Fit the initial logistic regression model to the original data
  rv$fit = glm(formula, data, family=binomial)
  
  # Figure out what the target variable is, and how many coefficients there are
  target = as.character(rv$fit$terms[[2]])
  rv$perm.coef = matrix(NA,R,length(fit$coefficients),dimnames=list(NULL,names(fit$coefficients)))
  
  # Do the permutations
  d = data
  for (i in 1:R)
  {
    # Permute the targets using sample(n) to generate a random ordering of
    # integers from 1:n
    d[,target] = d[sample(n),target]
    
    # Fit the model using the permuted data
    fit.perm = glm(formula, d, family=binomial)
    
    # Store the permuted coefficients into our matrix
    rv$perm.coef[i,] = fit.perm$coefficients
  }  

  # Compute the permutation p-values
  # To do this we first use "sweep" to check if the elements
  # of the abs(fit$coefficients) vector are greater than the 
  # corresponding elements of abs(rv$perm.coef) for each row, then
  # take the mean to get the proportion of times the permuted coefficients
  # are larger than the original fitted coefficients
  rv$p.value = colMeans( sweep(abs(rv$perm.coef), 2, abs(fit$coefficients), ">") )
  
  # this is an efficient implementation fo the following code:
  #
  # rv$p.value = vector(length=length(fit$coefficients))
  # for (j in 1:R)
  # {
  #   rv$p.value = rv$p.value + (abs(rv$perm.coef[j,]) > abs(fit$coefficients))
  # }
  # rv$p.value = rv$p.value / R
  
  return(rv)
}