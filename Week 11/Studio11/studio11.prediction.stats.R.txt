library(pROC)

my.pred.stats <- function(prob, target, display = TRUE)
{
  rv = list()
  
  classes = levels(target)
  
  # Convert probabilities to best guesses at classes
  pred = factor(prob > 1/2, c(F,T), classes)

  # Compute statistics
  T = table(pred,target)
  roc.obj = roc(response=as.numeric(target)-1, as.vector(prob), quiet=TRUE)
  rv$ca   = mean(pred==target)
  rv$sens = T[2,2]/(T[1,2]+T[2,2])
  rv$spec = T[1,1]/(T[1,1]+T[2,1])
  rv$auc  = as.numeric(roc.obj$auc)

  # Prob is probability of success, so if the target is not a success, flip the probability
  # to get probability of failure
  prob[target==classes[1]] = 1 - prob[target==classes[1]]
  # Also make sure we never get exactly zero or one for probabilities due to numerical rounding
  prob = (prob+1e-10)/(1+2e-10)

  rv$log.loss = -sum(log(prob))
  
  # Display, if requested    
  if (display == TRUE)
  {
    cat("---------------------------------------------------------------------------\n")
    cat("Performance statistics:\n")
    cat("\n")
    cat("Confusion matrix:\n\n")
    print(T)
    cat("\n")
    cat("Classification accuracy =", rv$ca, "\n")
    cat("Sensitivity             =", rv$sens, "\n")
    cat("Specificity             =", rv$spec, "\n")
    cat("Area-under-curve        =", rv$auc, "\n")

    cat("Logarithmic loss        =", rv$log.loss, "\n")
    cat("\n")
    
    #plot(roc.obj)
    
    cat("---------------------------------------------------------------------------\n")
  }
  else 
  {
    #
    return(rv)
  }
}
