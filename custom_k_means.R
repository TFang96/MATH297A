# X - input dataset
# K - cluster count
# improv_threshold - when do we stop
# maxItr - the maximum number of iterations we want to go through
custom_kmeans <- function(X, K, improv_threshold, maxItr) {
  # check for valid input and clusters
  if(K > nrow(X))
    stop("We cannot have more clusters than actual points")
  
  # choose K centers
  centerIndxs = sample.int(nrow(X), K)
  centerObs = X[centerIndxs, drop = FALSE]
  
  observation_labels <- integer(n) # this is the vector of labels
  E <- inf # this is the WSS
  
  # loop through the iterations
  for (i in 1:maxItr) {
    # compute and store distances from each observation to our centers
    distances <- sapply(1:K, function(k)
      rowSums((X - centerObs[k, , drop = TRUE])^2))
    
    # ensure distances is matrix
    if(is.null(dim(distances)))
      distances <- matrix(distances, ncol = K)
    
    # assign each observation to the closest center
    newLabels <- apply(distances, 1, which.min)
    
    #update the cluster centroids
    newCenters <- matrix(NA, nrow = K, ncol = p)
    # for every cluster
    for (k in 1:K) {
      members <- which(newLabels == k)
      #calculate new clusters
      newCenters[k, ] <- colMeans(X[members, , drop = FALSE])
    }
    
    #compute new wss (E)
    E_new <- 0
    for (k in 1:K) {
      members <- which(newLabels == k)
      E_new <- E_new + sum(rowSums((X[members, , drop = FALSE] - newCenters[k, ])^2))
    }
    
    if(is.infinite((E_ol)))
      improvement <- Inf
    else
      improvement <- E_old - E_new
    
    centerObs <- newCenters
    observation_labels <- newLabels
    
    # is the improvement at our threshold
    if(improvement != Inf && improvement < improv_threshold)
      break
  }
}