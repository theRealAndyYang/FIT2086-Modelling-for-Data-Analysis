####################################################################################
#	Script-file:   studio3.solns.R
#	Project:       FIT2086 - Studio 3 
#
# Purpose:  	   Solutions for questions in Studio 3
####################################################################################


# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------

cat("Question 3\n")

# 3.1
my_estimates <- function(X)
{
  n = length(X)
  
  retval = list()
  
  # Calculate the sample mean
  retval$mu_ml = sum(X)/n
  
  # Calculate the squared deviations around the mean
  e2 = (X - retval$m)^2
  
  # Calculate the two estimates of variance
  retval$var_ml = sum(e2)/n
  retval$var_u  = sum(e2)/(n-1)
  
  return(retval)
}

# 3.2 Estimate from training data
train <- read.csv("train.csv")

est = my_estimates(train$heights)
est$mu_ml
est$var_ml
est$var_u
sqrt(est$var_ml)
sqrt(est$var_u)
# => the unbiased estimate of variance is greater than the ML estimate

# 3.3 Recreate the Figure
plot(x=train$heights, y=rep(0,10), ylim=c(0,6), xlim=c(1.4,2), 
     ylab="p(heights)", xlab="Heights")
xv = seq(from=1.4, to=2, length.out=100)
lines(xv, dnorm(xv, est$mu_ml, sqrt(est$var_ml)), lwd=2.5, col="red")
lines(xv, dnorm(xv, est$mu_ml, sqrt(est$var_u)), lwd=2.5, col="blue")

# Tip: lty = 0 means no line, pch = "o" means a circle, pch = "" means no symbol
legend(x=1.75,y=6,c("Samples","ML Estimate","Unbiased Estimate"), lty=c(0,1,1), 
       pch=c("o","",""), col=c("black","red","blue"), lwd=c(1,2.5,2.5))

# 3.4 Empirical vs predicted probabilities
test <- read.csv("test.csv")

cat("P(X > 1.7m) =", 1-pnorm(1.7,est$mu_ml,sqrt(est$var_ml)), "(ML);", 
    1-pnorm(1.7,est$mu_ml,sqrt(est$var_u)), "(unbiased);", mean(test$heights>1.7), "(empirical)\n")

cat("P(X < 1.5m) =", pnorm(1.5,est$mu_ml,sqrt(est$var_ml)), "(ML);", 
    pnorm(1.5,est$mu_ml,sqrt(est$var_u)), "(unbiased);", mean(test$heights<1.5), "(empirical)\n")

cat("P(X > 1.6m & X < 1.75m) =", pnorm(1.75,est$mu_ml,sqrt(est$var_ml)) - pnorm(1.6,est$mu_ml,sqrt(est$var_ml)), "(ML);", 
    pnorm(1.75,est$mu_ml,sqrt(est$var_u))-pnorm(1.6,est$mu_ml,sqrt(est$var_u)), "(unbiased);", mean(test$heights > 1.6 & test$heights < 1.75), "(empirical)\n")

# Unbiased estimate seems a bit better in this case, but does appear to overestimate 
# the probability of people being taller than 1.7m -- this is more likely for unbiased
# as it tends to estimate a bigger spread.

# 3.5 Empirical negative log-likelihood
norm_negloglike <- function(y,mu,sigma)
{
  n = length(y)
  return( (n/2)*log(2*pi*sigma^2) + 1/2/sigma^2*sum((y-mu)^2) )
}
cat("Neg-log-likelihood (unbiased):", norm_negloglike(test$heights, est$mu_ml, sqrt(est$var_ml)))
cat("; Neg-log-likelihood (ML):", norm_negloglike(test$heights, est$mu_ml, sqrt(est$var_u)),'\n')


# ----------------------------------------------------------------------------------
# Question 4
# ----------------------------------------------------------------------------------

cat("\nQuestion 5\n")

# 5.1
mean_median_test <- function(niterations, mu, sigma, n)
{
  mu_hat = rep(0,niterations)
  med_hat = rep(0,niterations)
  
  # Accumulate statistics
  for (i in 1:niterations)
  {
    # Generate data
    y = rnorm(n, mu, sigma)
    
    # Calculate sample mean and median
    mu_hat[i] = mean(y)
    med_hat[i] = median(y)
  }
  
  # Done
  retval = list()
  retval$bias_mean = mean( mu_hat - mu )
  retval$var_mean = var( mu_hat )
  retval$mse_mean = retval$bias_mean^2 + retval$var_mean
  
  retval$bias_med = mean( med_hat - mu )
  retval$var_med = var( med_hat )
  retval$mse_med = retval$bias_med^2 + retval$var_med
  
  retval$rel_mse = retval$mse_mean / retval$mse_med
  
  return(retval)
}

# 5.2
rv = mean_median_test(1e4, 0, 1, 10)
rv
# neither of the estimators appear biased, median has higher variance (and mse)

cat("Simulation vs exact\n")
cat("Simulation: bias:", rv$bias_mean, "variance:", rv$var_mean, "mse:", rv$mse_mean,"\n")
cat("Exact:      bias: 0, variance: 0.1, mse: 0.1\n")

# 5.3
rv = mean_median_test(1e4, 0, 10, 10)
rv
# variance goes up by factor of approx 100 (10^2) for both, so relative mse stays the same

# 5.4
rv = mean_median_test(1e4, 0, 10, 100)
rv
# variance goes down by 10 for mean but goes down by less than factor of 10 for median
# => relative mse changes (gets better in favour of mean)

# 5.5 Modified function
mean_median_test <- function(niterations, mu, sigma, n, nc = 0)
{
  mu_hat = rep(0,niterations)
  med_hat = rep(0,niterations)
  
  # Accumulate statistics
  for (i in 1:niterations)
  {
    # Generate data
    y = rnorm(n, mu, sigma)
    
    # If contamination is requested
    if (nc > 0)
    {
      # Replace the first 'nc' points of the sample
      y[1:nc] = rnorm(nc, mu, 4*sigma)
    }
    
    # Calculate sample mean and median
    mu_hat[i] = mean(y)
    med_hat[i] = median(y)
  }
  
  # Done
  retval = list()
  retval$bias_mean = mean( mu_hat - mu )
  retval$var_mean = var( mu_hat )
  retval$mse_mean = retval$bias_mean^2 + retval$var_mean
  
  retval$bias_med = mean( med_hat - mu )
  retval$var_med = var( med_hat )
  retval$mse_med = retval$bias_med^2 + retval$var_med
  
  retval$rel_mse = retval$mse_mean / retval$mse_med
  
  return(retval)
}

# Test for nc = 1 - mean is now worse relative to median
rv = mean_median_test(1e4, mu=0, sigma=1, n=10, nc=1)
rv$rel_mse

# Test for nc = 2 - mean getting worse
rv = mean_median_test(1e4, mu=0, sigma=1, n=10, nc=2)
rv$rel_mse

# Test for nc = 2 - with enough samples mean can be made better again
rv = mean_median_test(1e4, mu=0, sigma=1, n=50, nc=2)
rv$rel_mse

# Test for nc = 4 - but contaminate a few more points and it quickly gets worse ...
rv = mean_median_test(1e4, mu=0, sigma=1, n=50, nc=4)
rv$rel_mse

