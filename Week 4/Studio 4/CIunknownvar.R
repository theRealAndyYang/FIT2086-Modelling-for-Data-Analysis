# Function to estimate the mean and variance from a sample of data
# Also returns a 100(1-alpha) confidence interval for the mean
calcCI <- function(y, alpha)
{
  n = length(y)
  
  retval = list()
  
  # Simple error checking
  if (alpha <= 0 || alpha >= 1)
  {
    stop("Alpha must be a value greater than 0 and less than 1")
  }
  
  # Calculate the sample mean and (unbiased) estimate of variance
  retval$mu.hat = mean(y)
  
  # the "var()" function returns the unbiased estimate of variance
  retval$sigma2.hat = var(y) 
  
  # Calculate the multiplier for our CI based on t-distribution (unknown variance CI)
  t = qt(1-alpha/2, n-1)
  
  # return the interval
  retval$CI = retval$mu.hat + c(-t * sqrt(retval$sigma2.hat/n), 
                                t * sqrt(retval$sigma2.hat/n))
  return(retval)
}