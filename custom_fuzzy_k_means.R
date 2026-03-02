# X - input dataset (numeric matrix or data.frame of numeric cols)
# K - cluster count
# v - fuzziness (>1), common choice v = 2
# improv_threshold - stopping threshold for improvement in objective
# maxItr - maximum iterations
custom_fuzzy_kmeans <- function(X, K, v = 2, improv_threshold = 1e-6, maxItr = 100) {
  # make sure X is numeric matrix
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  
  # validation checks
  if (K > nrow(X)) stop("We cannot have more clusters than actual points")
  if (K <= 0) stop("We cannot have negative clusters")
  
  n <- nrow(X)
  p <- ncol(X)
  
  # initialize U matrix
  U <- matrix(runif(n * K), nrow = n, ncol = K) # randomly generate membership values for each observation
  U <- U / rowSums(U) # make sure membership grades sum up to for every observation across the clusters. 
  
  E_old <- Inf
  errors <- c()
  
  for (iter in 1:maxItr) { # iterate across all iterations 
    # --- update centers using weighted means with U^v
    U_v <- U^v # raise every membership to the power of v, the fuzzifier
    denom <- colSums(U_v) # the denominator in calculating the centers
    
    newCenters <- matrix(0, nrow = K, ncol = p)
    for (k in 1:K) {
      # calculate the new clusters using the formula: sum(u_ik ^ v * x_i) / sum(u_ik ^ v)
      newCenters[k, ] <- colSums(X * U_v[, k]) / denom[k]
    }
    
    # --- compute squared distances D: n x K
    distances <- sapply(1:K, function(k)
      rowSums((X - newCenters[k, , drop = TRUE])^2))
    distances <- as.matrix(distances)
    
    # --- update membership matrix U
    U_new <- matrix(0, nrow = n, ncol = K)
    
    for (i in 1:n) {
      d_i <- distances[i, ]
      
      # point is exactly at center
      if (any(d_i == 0)) {
        k0 <- which.min(d_i)
        U_new[i, ] <- 0
        U_new[i, k0] <- 1
      } else {
        # Using squared distances, the exponent is 1/(m-1)
        # u_ik = 1 / sum_j ( (d_ik / d_ij)^(1/(m-1)) )
        for (k in 1:K) {
          U_new[i, k] <- 1 / sum((d_i[k] / d_i)^(1 / (m - 1)))
        }
      }
    }
    
    # --- compute fuzzy objective J_m
    E_new <- sum((U_new^v) * distances)
    
    # improvement (same sign convention as your code)
    if (is.infinite(E_old))
      improvement <- Inf
    else
      improvement <- E_old - E_new
    
    # update state
    U <- U_new
    centers <- newCenters
    
    if (improvement != Inf && improvement < improv_threshold)
      break
    
    E_old <- E_new
    errors <- append(errors, E_old)
  }
  
  return(list(
    membership = U,
    
    centers = centers,
    errors = errors,
    v = v,
    iter = iter
  ))
}