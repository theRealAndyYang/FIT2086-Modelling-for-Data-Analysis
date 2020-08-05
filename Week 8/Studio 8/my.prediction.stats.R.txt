library(pROC)

my.pred.stats <- function(prob, target)
{
  classes = levels(target)
  
  # Convert probabilities to best guesses at classes
  pred = factor(prob > 1/2, c(F,T), classes)
  
  cat("---------------------------------------------------------------------------\n")
  cat("Performance statistics:\n")
  cat("\n")
  cat("Confusion matrix:\n\n")
  T = table(pred,target)
  print(T)
  cat("\n")
  cat("Classification accuracy =", mean(pred==target), "\n")
  cat("Sensitivity             =", T[2,2]/(T[1,2]+T[2,2]), "\n")
  cat("Specificity             =", T[1,1]/(T[1,1]+T[2,1]), "\n")
  roc.obj = roc(response=as.numeric(target)-1, prob)
  cat("Area-under-curve        =", roc.obj$auc, "\n")
  
  # Prob is probability of success, so if the target is not a success, flip the probability
  # to get probability of failure
  prob[target==classes[1]] = 1 - prob[target==classes[1]]
  # Also make sure we never get exactly zero or one for probabilities due to numerical rounding
  prob = (prob+1e-10)/(1+2e-10)
  
  cat("Logarithmic loss        =", -sum(log(prob)), "\n")
  cat("\n")
  
  plot(roc.obj)
  
  cat("---------------------------------------------------------------------------\n")
}
