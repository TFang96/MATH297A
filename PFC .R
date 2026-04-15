###############################################################################
# Power Fuzzy Clustering (PFC) 
#
# Implements power fuzzy clustering with support for:
#   - Minkowski distance (p = 1 and p > 1)
#   - Mahalanobis distance (with regularisation)
#   - Cluster-wise linear regression (PFCR) and plain clustering (PFC)
#
# Dependency: lpSolve
###############################################################################

library(lpSolve)


###############################################################################
# PFCR – Power Fuzzy Cluster-wise Regression (wrapper)
#
# Fits K cluster-specific linear models  Y = X %*% B_k + error,
# selecting the appropriate internal solver based on distance type and p.
#
# Arguments:
#   X          - n x dx data.frame or matrix of covariates
#   Y          - n x dy data.frame or matrix of dependent variables
#   K          - number of clusters (positive integer)
#   m          - fuzzifier (> 1, default 2)
#   q          - distance exponent (> 0, default 2)
#   distance   - 'Minkowski' or 'Mahalanobis'
#   p          - Minkowski exponent (>= 1, default 2; ignored for Mahalanobis)
#   alpha      - regularisation weight for Mahalanobis covariance (default 0.5)
#   beta       - eigenvalue ratio bound for Mahalanobis covariance (default 1e15)
#   threshold  - convergence tolerance (default 0.01)
#   max.iter   - maximum number of iterations (default 100)
#
# Returns a list with:
#   B   - array of regression coefficients
#   d   - data.frame of distances (n x K)
#   p   - data.frame of membership degrees (n x K)
#   JDF - vector of objective-function values per iteration
#   l   - hard cluster labels (length n)
#   rho, cov - (Mahalanobis only) cluster proportions and covariance matrices
###############################################################################

###############################################################################
# PFC – Power Fuzzy Clustering (wrapper, no covariates)
#
# Clusters the rows of Y into K groups.
# Arguments and returns: same as PFCR except X is not needed.
###############################################################################

PFC <- function(Y, K, m = 2, q = 2, distance = 'Mahalanobis', p = 2,
                alpha = 0.5, beta = 10^15, threshold = 0.01, max.iter = 100) {
  
  # --- Input validation ---
  if (distance != 'Minkowski' & distance != 'Mahalanobis') {
    stop("'distance' must be either 'Mahalanobis' or 'Minkowski'.")
  }
  if (!is.numeric(K) || length(K) != 1 || K < 1 || K != round(K)) {
    stop("'K' must be a positive integer.")
  }
  if (m <= 1)  stop("'m' (fuzzifier) must be strictly greater than 1.")
  if (q <= 0)  stop("'q' (distance exponent) must be strictly greater than 0.")
  if (p < 1)   stop("'p' (Minkowski exponent) must be >= 1.")
  if (nrow(as.matrix(Y)) < K) {
    stop("Number of observations must be >= K.")
  }
  
  a   <- m
  b   <- q
  pow <- p
  X   <- NULL
  
  if (distance == 'Minkowski') {
    if (pow == 1) {
      res <- PDVWithMinkowskiEqualToOne(Y = Y, K = K, a = a, b = b,
                                        threshold, max.iter)
    } else {
      res <- PDVWithMinkowskiMoreThanOne(Y = Y, K = K, a = a, b = b,
                                         pow = pow, threshold, max.iter)
    }
  } else {
    res <- PDVRegressionWithCovar(X = X, Y = Y, K = K, a = a, b = b,
                                  alpha, beta, threshold, max.iter)
  }
  
  return(res)
}


###############################################################################
# 1. Mahalanobis-based cluster-wise regression
#
# Uses an adaptive, regularised covariance matrix per cluster.
# Also serves as the Mahalanobis clustering solver when X = NULL
# (intercept-only design).
###############################################################################

PDVRegressionWithCovar <- function(X = NULL, Y, K, a, b, alpha, beta,
                                   threshold = 0.01, max.iter = 100) {
  
  # --- Prepare design matrix (add intercept) ---
  if (is.null(X)) {
    X <- matrix(1, nrow(Y), 1)
  } else {
    X <- as.matrix(X)
    X <- cbind(rep(1, nrow(X)), X)
  }
  Y <- as.matrix(Y)
  
  const <- b / (a - 1)
  
  dx <- ncol(X)
  dy <- ncol(Y)
  n  <- nrow(X)
  
  # --- Initialise regression coefficients ---
  B    <- array(runif(dx * dy * K), dim = c(dx, dy, K))
  newB <- B
  if (dx == 1) {
    B    <- array(t(kmeans(Y, K)$centers), dim = c(dx, dy, K))
    newB <- B
  }
  
  # --- Initialise covariance components ---
  omega <- array(dim = c(dy, dy, K))
  rho   <- rep(1 / K, K)
  covs  <- array(dim = c(dy, dy, K))
  
  for (i in 1:K) {
    omega[, , i] <- diag(dy)
  }
  for (i in 1:K) {
    covs[, , i] <- rho[i] * omega[, , i]
  }
  
  # --- Initialise distance and membership matrices ---
  d    <- data.frame(matrix(ncol = K, nrow = n))
  newD <- data.frame(matrix(ncol = K, nrow = n))
  p    <- data.frame(matrix(0, ncol = K, nrow = n))
  
  for (x in 1:K) {
    names(d)[x] <- paste("Cluster", x)
    names(p)[x] <- paste("Cluster", x)
  }
  
  JDF     <- c()
  newNorm <- threshold + 1
  iter    <- 0
  
  while (newNorm >= threshold && iter < max.iter) {
    iter <- iter + 1
    
    # --- Compute Mahalanobis distances ---
    for (j in 1:K) {
      inv <- solve(covs[, , j])
      for (i in 1:n) {
        resids  <- t(matrix(unlist(Y[i, ]))) - t(matrix(unlist(X[i, ]))) %*% B[, , j]
        d[i, j] <- (resids %*% inv %*% t(resids))^(1 / 2)
      }
    }
    
    # --- Compute objective function (JDF) ---
    JDF_value <- 0
    for (x in 1:n) {
      JDF_numerator   <- prod(d[x, ])^const
      JDF_denominator <- 0
      for (y in 1:K) {
        JDF_denominator <- JDF_denominator + prod(d[x, -y])^const
      }
      JDF_value <- JDF_value + (JDF_numerator / JDF_denominator)^(a - 1)
    }
    JDF <- c(JDF, JDF_value)
    
    # --- Compute membership degrees ---
    for (x in 1:n) {
      sum1 <- 0
      for (y in 1:K) {
        sum1 <- sum1 + prod(d[x, -y])^const
      }
      for (z in 1:K) {
        product1  <- prod(d[x, -z])^const
        p[x, z]   <- product1 / sum1
      }
    }
    p[sapply(p, is.nan)] <- 1
    
    # --- Update regression coefficients (weighted least squares) ---
    for (j in 1:K) {
      ans1 <- matrix(0, ncol = dy, nrow = dx)
      ans2 <- matrix(0, ncol = dx, nrow = dx)
      for (i in 1:n) {
        ans1 <- ans1 + p[i, j]^a * d[i, j]^(b - 2) *
          (matrix(unlist(X[i, ])) %*% t(matrix(unlist(Y[i, ]))))
        ans2 <- ans2 + p[i, j]^a * d[i, j]^(b - 2) *
          (matrix(unlist(X[i, ])) %*% t(matrix(unlist(X[i, ]))))
      }
      newB[, , j] <- solve(ans2) %*% ans1
    }
    
    # --- Recompute distances with omega inverse (shape only) ---
    for (j in 1:K) {
      invO <- solve(omega[, , j])
      for (i in 1:n) {
        resids     <- t(matrix(unlist(Y[i, ]))) - t(matrix(unlist(X[i, ]))) %*% B[, , j]
        newD[i, j] <- (resids %*% invO %*% t(resids))^(1 / 2)
      }
    }
    
    # --- Update scatter matrices ---
    S <- array(dim = c(dy, dy, K))
    for (j in 1:K) {
      ans <- matrix(0, ncol = dy, nrow = dy)
      for (i in 1:n) {
        ans1 <- t(matrix(unlist(Y[i, ]))) - t(matrix(unlist(X[i, ]))) %*% B[, , j]
        ans  <- ans + p[i, j]^a * newD[i, j]^(b - 2) * (t(ans1) %*% ans1)
      }
      S[, , j] <- ans
    }
    
    # --- Shape matrix (unit determinant) ---
    for (j in 1:K) {
      omega[, , j] <- S[, , j] / det(S[, , j])^(1 / dy)
    }
    
    # --- Cluster proportions rho ---
    sum <- 0
    for (j in 1:K) {
      sum1 <- 0
      for (i in 1:n) {
        sum1 <- sum1 + newD[i, j]^b %*% p[i, j]^a
      }
      sum <- sum + (sum1)^(2 / (b + 2))
    }
    for (j in 1:K) {
      sum2 <- 0
      for (i in 1:n) {
        sum2 <- sum2 + newD[i, j]^b %*% p[i, j]^a
      }
      rho[j] <- (sum2)^(2 / (b + 2)) / sum
    }
    
    # --- Assemble and regularise covariance ---
    for (j in 1:K) {
      covs[, , j] <- rho[j] * omega[, , j]
    }
    for (j in 1:K) {
      covs[, , j] <- (1 - alpha) * covs[, , j] +
        alpha * det(cov(cbind(X, Y)))^(1 / dy) * diag(dy)
    }
    
    # --- Eigenvalue ratio bound ---
    for (j in 1:K) {
      eigen_decomp <- eigen(omega[, , j])
      eigenvalues  <- eigen_decomp$values
      eigenvectors <- eigen_decomp$vectors
      max_eigen    <- max(eigenvalues)
      
      for (s in 1:dy) {
        if (max_eigen > beta * eigenvalues[s]) {
          eigenvalues[s] <- max_eigen / beta
        }
      }
      covs[, , j] <- eigenvectors %*% diag(eigenvalues) %*% solve(eigenvectors)
    }
    
    # --- Convergence check ---
    newNorm <- sqrt(sum(B - newB)^2)
    B <- newB
  }
  
  return(list(B = B, d = d, p = p, JDF = JDF, rho = rho, cov = covs,
              l = max.col(p)))
}







###############################################################################
#                           CLUSTERING (no covariates)
###############################################################################


###############################################################################
# 4. Clustering – Minkowski  (p > 1)
#
# Intercept-only design: B represents cluster centres.
# Uses uniroot to solve first-order conditions for each centre coordinate.
###############################################################################

PDVWithMinkowskiMoreThanOne <- function(Y, K, a, b, pow, threshold, max.iter) {
  
  X <- matrix(1, nrow(Y), 1)
  Y <- as.matrix(Y)
  
  const <- b / (a - 1)
  
  dx <- ncol(X)
  dy <- ncol(Y)
  n  <- nrow(X)
  
  B    <- array(t(kmeans(Y, K)$centers), dim = c(dy, dx, K))
  newB <- B
  
  d <- data.frame(matrix(ncol = K, nrow = n))
  p <- data.frame(matrix(0, ncol = K, nrow = n))
  
  for (x in 1:K) {
    names(d)[x] <- paste("Cluster", x)
    names(p)[x] <- paste("Cluster", x)
  }
  
  JDF     <- c()
  newNorm <- threshold + 1
  iter    <- 0
  
  # --- FOC helper for centre coordinate s of cluster j ---
  f_i_1 <- function(i, j, s, x) {
    term <- p[i, j]^a / d[i, j]^(pow - b)
    u    <- (Y[i, s] - x)
    return(term * abs(u)^(pow - 2) * u)
  }
  
  f_1 <- function(j, s, x) {
    total_sum <- 0
    for (i in 1:n) {
      total_sum <- total_sum + f_i_1(i, j, s, x)
    }
    return(total_sum)
  }
  
  while (newNorm >= threshold & iter < max.iter) {
    iter <- iter + 1
    
    # --- Compute Minkowski distances ---
    for (j in 1:K) {
      for (i in 1:n) {
        resids  <- cbind(matrix(unlist(Y[i, ])),
                         B[, , j] %*% matrix(unlist(X[i, ])))
        d[i, j] <- sum(abs(resids[, 1] - resids[, 2])^pow)^(1 / pow)
      }
    }
    
    # --- Compute objective function (JDF) ---
    JDF_value <- 0
    for (x in 1:n) {
      JDF_numerator   <- prod(d[x, ])^const
      JDF_denominator <- 0
      for (y in 1:K) {
        JDF_denominator <- JDF_denominator + prod(d[x, -y])^const
      }
      JDF_value <- JDF_value + (JDF_numerator / JDF_denominator)^(a - 1)
    }
    JDF <- c(JDF, JDF_value)
    
    # --- Compute membership degrees ---
    for (x in 1:n) {
      sum1 <- 0
      for (y in 1:K) {
        sum1 <- sum1 + prod(d[x, -y])^const
      }
      for (z in 1:K) {
        product1 <- prod(d[x, -z])^const
        p[x, z]  <- product1 / sum1
      }
    }
    p[sapply(p, is.nan)] <- 1
    
    # --- Solve for centre coordinates ---
    for (j in 1:K) {
      for (s in 1:dy) {
        newB[s, 1, j] <- uniroot(function(x) f_1(j, s, x),
                                 c(-1e18, 1e18), tol = 0.0001)$root
      }
    }
    
    B <- newB
  }
  
  l <- apply(p, 1, which.max)
  return(list(C = B, d = d, p = p, JDF = JDF, l = l))
}


###############################################################################
# 5. Clustering – Minkowski  (p = 1)
#
# L1 centres via IRLS (weighted median-like update).
###############################################################################

PDVWithMinkowskiEqualToOne <- function(Y, K, a, b, threshold, max.iter) {
  
  X <- matrix(1, nrow(Y), 1)
  Y <- as.matrix(Y)
  
  const <- b / (a - 1)
  
  dx <- ncol(X)
  dy <- ncol(Y)
  n  <- nrow(X)
  
  B    <- array(t(kmeans(Y, K)$centers), dim = c(dy, dx, K))
  newB <- B
  
  d <- data.frame(matrix(ncol = K, nrow = n))
  p <- data.frame(matrix(0, ncol = K, nrow = n))
  
  for (x in 1:K) {
    names(d)[x] <- paste("Cluster", x)
    names(p)[x] <- paste("Cluster", x)
  }
  
  JDF     <- c()
  newNorm <- threshold + 1
  iter    <- 0
  
  while (newNorm >= threshold & iter < max.iter) {
    iter <- iter + 1
    
    # --- Compute L1 distances ---
    for (j in 1:K) {
      for (i in 1:n) {
        resids  <- cbind(matrix(unlist(Y[i, ])),
                         B[, , j] %*% matrix(unlist(X[i, ])))
        d[i, j] <- sum(abs(resids[, 1] - resids[, 2])^1)^(1 / 1)
      }
    }
    
    # --- Compute objective function (JDF) ---
    JDF_value <- 0
    for (x in 1:n) {
      JDF_numerator   <- prod(d[x, ])^const
      JDF_denominator <- 0
      for (y in 1:K) {
        JDF_denominator <- JDF_denominator + prod(d[x, -y])^const
      }
      JDF_value <- JDF_value + (JDF_numerator / JDF_denominator)^(a - 1)
    }
    JDF <- c(JDF, JDF_value)
    
    # --- Compute membership degrees ---
    for (x in 1:n) {
      sum1 <- 0
      for (y in 1:K) {
        sum1 <- sum1 + prod(d[x, -y])^const
      }
      for (z in 1:K) {
        product1 <- prod(d[x, -z])^const
        p[x, z]  <- product1 / sum1
      }
    }
    p[sapply(p, is.nan)] <- 1
    
    # --- IRLS centre update ---
    for (j in 1:K) {
      for (s in 1:dy) {
        numerator   <- 0
        denominator <- 0
        for (i in 1:n) {
          abs_resid   <- abs(Y[i, s] - newB[s, 1, j])
          abs_resid   <- max(abs_resid, 1e-18)  # avoid division by zero
          w           <- (p[i, j]^a * d[i, j]^(b - 1)) / abs_resid
          numerator   <- numerator + w * Y[i, s]
          denominator <- denominator + w
        }
        newB[s, 1, j] <- numerator / denominator
      }
    }
    
    newNorm <- sqrt(sum(B - newB)^2)
    B <- newB
  }
  
  l <- apply(p, 1, which.max)
  return(list(B = B, d = d, p = p, JDF = JDF, l = l))
}
