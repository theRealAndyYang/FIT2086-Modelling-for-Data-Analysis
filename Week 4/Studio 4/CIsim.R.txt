testCIknownSigma2 <- function(pop.mu, pop.sigma2, n, niter)
{
  retval = list()
  retval$coverage = 0
  
  # Do niter simulations
  for (i in 1:niter)
  {
    # Generate data from the population
    y = rnorm(n, pop.mu, sqrt(pop.sigma2))
    
    # Compute the 95% confidence interval
    mu.hat = mean(y)
    CI = mu.hat + c(-1.96*sqrt(pop.sigma2)/sqrt(n), 1.96*sqrt(pop.sigma2)/sqrt(n))
    
    # Does it cover the population parameter?
    if (pop.mu >= CI[1] && pop.mu <= CI[2])
    {
      retval$coverage = retval$coverage + 1
    }
  }
  
  # Estimate coverage as proportion of times interval covered the population parameter
  retval$coverage = retval$coverage/niter
  
  return(retval)
}