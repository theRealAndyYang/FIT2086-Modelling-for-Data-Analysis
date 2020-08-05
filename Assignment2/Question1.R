getwd()
setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Assignment2")

### 1 ###
dogbite <- read.csv("dogbites.fullmoon.csv", header = TRUE)
dogbite_fullmoon <- dogbite[dogbite$is.full.moon == 1,]

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

my_estimates(dogbite_fullmoon$daily.dogbites)




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

est = calcCI(dogbite_fullmoon$daily.dogbites, 0.05)

est$mu.hat  # Estimated mean
est$CI 


### 2 ###
dogbite_notfullmoon <- dogbite[dogbite$is.full.moon == 0,]

est_fullmoon = calcCI(dogbite_fullmoon$daily.dogbites, alpha=0.05)
est_notfullmoon = calcCI(dogbite_notfullmoon$daily.dogbites, alpha=0.05)

n1 = length(dogbite_fullmoon)
n2 = length(dogbite_notfullmoon)

# get the difference
diff = est_fullmoon$mu.hat - est_notfullmoon$mu.hat

# calculate standard error
se.diff = sqrt(est_fullmoon$sigma2.hat/n1 + est_notfullmoon$sigma2.hat/n2)

# calculate the 95% CI
CI.diff = diff + c(-1.96*se.diff, 1.96*se.diff)

diff
CI.diff


### 3 ###
dogbite_1<-dogbite[dogbite[,2]==1,1]
dogbite_0<-dogbite[dogbite[,2]==0,1]

t.test(dogbite_1,dogbite_0,var.equal = F)

