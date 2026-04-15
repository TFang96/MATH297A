xb_calc <- function(centers, membership, distances, covariance, m, q, clusterCnt) {
  num <- 0
  for(i in 1:nrow(distances)) {
    for(j in 1:ncol(distances))
      num <- num + (membership[i,j]^m) * (distances[i, j]^q)
  }
  
  ## calculate membership sums
  
  membership_sum <- 0 
  for(i in 1:nrow(membership)) {
    for(j in 1:ncol(membership))
      membership_sum <- membership_sum + membership[i, j]
  }
  
  num <- num/membership_sum
  
  denom <- Inf
  u_matrix <- as.matrix(membership)
  
  for(j in 1:(clusterCnt - 1)) {
    for(l in (j+1):clusterCnt) {
      
      ## calculate centers
      cj <- as.vector(centers[1, ,j])
      cl <- as.vector(centers[1, ,l])
      
      # calculate cluster sizes
      nj <- sum(u_matrix[, j])
      nl <- sum(u_matrix[, l])
      
      Sigma_jl <- (nj * covariance[, ,j] + nl * covariance[, , l]) / (nj + nl)
      
      diff <- matrix(cl - cj, ncol = 1)
      
      sep <- as.numeric(t(diff) %*% solve(Sigma_nl) %*% diff)
      
      denom <- min(denom, sep)
    }
  }
  return(num/denom)
}