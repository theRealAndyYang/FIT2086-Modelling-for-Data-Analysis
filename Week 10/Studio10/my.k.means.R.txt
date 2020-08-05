my.k.means <- function(X, k)
{
  # Convert the dataframe to a matrix
  X = as.matrix(X)
  
  q = ncol(X)
  n = nrow(X)
  
  # Pre-allocate matrix for distances
  d  = matrix(ncol = k, nrow = n)
  
  # Initial centroids
  mu = X[sample(n)[1:k], ]
  
  # cluster ...
}
