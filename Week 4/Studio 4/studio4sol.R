####################################################################################
#	Script-file:   studio4.solns.R
#	Project:       FIT2086 - Studio 4
# Author:        Daniel Schmidt
#
# Purpose:  	   Solutions for questions in Studio 4
####################################################################################

setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Week 4/Studio 4")

source("CIunknownvar.R")

# Q2.8
train = read.csv("train.csv")

# Get the estimates and 95% (alpha = 0.05) confidence interval
est = calcCI(train$heights, 0.05)

est$mu.hat  # Estimated mean
est$CI      # CI for the mean

# Q2.9
test = read.csv("test.csv")

# Note the population mean is inside the CI from our small training sample
mean(test$heights)


# -------------------------------------------------------------------------------------------
# Q3.1
SP500 = read.csv("SP500.csv")
plot((SP500$Index), type="l", xlab="Week since 7th September, 2007", ylab="S&P Index", lwd=2.5)

# Q3.2
# If you look at the data, you will see week 58 is September 26th, 2008. 
# So first group is week 1 through 58, and second group is week 59 through 108

# First, colour the second group differently on our plot
lines(x=59:108,y=SP500$Index[59:108], col="red", lwd=2.5)

y1 = SP500$Index[1:58]
y2 = SP500$Index[59:108]

# Get our two estimates for the two groups
estG1 = calcCI(y1, alpha=0.05)
estG1
estG2 = calcCI(y2, alpha=0.05)
estG2

# Notice the two intervals do not overlap

# Q3.3 
# We need to use the approximate procedure for CI as we assume variances are unknown and not
# necessarily the same

n1 = length(y1)
n2 = length(y2)

# first get the difference
diff = estG1$mu.hat - estG2$mu.hat

# get the standard error of the difference
se.diff = sqrt( estG1$sigma2.hat/n1 + estG2$sigma2.hat/n2 )

# calculate the 95% CI
CI.diff = diff + c(-1.96*se.diff, 1.96*se.diff)

diff
CI.diff

# the interval for the difference is completely positive and very far away from zero, suggesting
# that the population difference between the two groups is not zero (i.e., there is a
# difference in S&P index pre- and post-Lehman Brothers investment bank collapse). See PDF 
# file for a stament of these results


# -------------------------------------------------------------------------------------------
# Q5.1
source("CIsim.R")
testCIknownSigma2(pop.mu=0, pop.sigma2=1, n=5, niter=1e4)
testCIknownSigma2(pop.mu=2, pop.sigma2=1, n=5, niter=1e4)
testCIknownSigma2(pop.mu=2, pop.sigma2=5, n=5, niter=1e4)
testCIknownSigma2(pop.mu=0, pop.sigma2=1, n=25, niter=1e4)

# We don't expect the results to change as for this problem (mean of normal with known variance)
# this procedure produces exact 95% confidence intervals

# Q5.2
## Modified function
testCIunknownSigma2 <- function(pop.mu, pop.sigma2, n, niter)
{
  retval = list()
  retval$coverage.approx = 0
  retval$coverage.t = 0
  
  # Do niter simulations
  for (i in 1:niter)
  {
    # Generate data from the population
    y = rnorm(n, pop.mu, sqrt(pop.sigma2))
    
    # Approximate 95% CI
    # Compute the approximate 95% confidence interval
    mu.hat = mean(y)
    sigma2.hat = var(y)
    se = sqrt(sigma2.hat)/sqrt(n)
    CI = mu.hat + c(-1.96*se, 1.96*se)
    
    # Does it cover the population parameter?
    if (pop.mu >= CI[1] && pop.mu <= CI[2])
    {
      retval$coverage.approx = retval$coverage.approx + 1
    }
    
    # Exact 95% CI using t-distribution (alpha=0.05, dof = sample size minus 1)
    # See Lecture 4
    t.alpha = qt(p = 1-0.05/2, df=n-1)
    CI = mu.hat + c(-t.alpha*se, t.alpha*se)
    
    # Does it cover the population parameter?
    if (pop.mu >= CI[1] && pop.mu <= CI[2])
    {
      retval$coverage.t = retval$coverage.t + 1
    }
  }
  
  # Estimate coverage as proportion of times interval covered the population parameter
  retval$coverage.approx = retval$coverage.approx/niter
  retval$coverage.t = retval$coverage.t/niter
  
  return(retval)
}

# Again, the coverage doesn't depend on either pop.mu or pop.sigma2
testCIunknownSigma2(pop.mu = 0, pop.sigma2 = 1, n = 5, niter = 1e4)
testCIunknownSigma2(pop.mu = 10, pop.sigma2 = 1, n = 5, niter = 1e4)
testCIunknownSigma2(pop.mu = 10, pop.sigma2 = 10, n = 5, niter = 1e4)

# But it does depend on the sample size ...
coverage.approx = rep.int(0,98)
coverage.t = rep.int(0,98)
for (n in 3:100)
{
  # Do the simulation
  rv = testCIunknownSigma2(0, 1, n, 1e4)
  
  # Store the results
  coverage.approx[n-2] = rv$coverage.approx
  coverage.t[n-2] = rv$coverage.t
}

# Plot the results
plot(x=3:100, y=coverage.approx, type="l", lwd = 2.5, col="black", ylab = "Coverage", xlab = "Sample Size")
lines(x=3:100, y=coverage.t, col="red", lwd = 2.5)
legend(x=60,y=0.85,c("Approximate 95% CI","Exact 95% CI (t-dist)"), 
       lty=c(1,1), lwd=c(2.5,2.5,2.5), col=c("black","red") )

# It is clear that as n gets larger, the two methods give the same level of coverage
# but for smaller sample sizes the approximate method is not particularly good

# Q5.3
## Modified function for Poisson populations
testCIlambda <- function(pop.lambda,n, niter)
{
  retval = list()
  retval$coverage = 0
  
  # Do niter simulations
  for (i in 1:niter)
  {
    # Generate data from the population
    y = rpois(n, pop.lambda)
    
    # Approximate 95% CI
    # Compute the approximate 95% confidence interval
    lambda.hat = mean(y)
    se = sqrt(lambda.hat)/sqrt(n)
    CI = lambda.hat + c(-1.96*se, 1.96*se)
    
    # Does it cover the population parameter?
    if (pop.lambda >= CI[1] && pop.lambda <= CI[2])
    {
      retval$coverage = retval$coverage + 1
    }
  }
  
  # Estimate coverage as proportion of times interval covered the population parameter
  retval$coverage = retval$coverage/niter
  
  return(retval)
}

# Test coverage for combos of lambda and sample size n
M = matrix(NA,4,5)
L = c(1,5,10,50)
N = c(5,10,25,50,100)
for (i in (1:4))
{
  for (j in (1:5))
  {
    M[i,j] = testCIlambda(L[i],N[j],1e5)$coverage
  }
}

# Turn into a dataframe so we can label rows/columns for easier reading
results = data.frame(M)
row.names(results) <- c("lambda=1","lambda=5","lambda=10","lambda=50")
names(results) <- c("n=5","n=10","n=25","n=50","n=100")

results

# This shows that the approximate CI for Poisson does not work that well only when
# (lambda = 1, n = 5) and is a little too small for (lambda = 5, n = 5)
# Otherwise it attains essentially 95% coverage, which is quite impressive
# This is due to the fact that the central limit theorem is working twice for the Poisson,
# as it applies as both lambda and n increase