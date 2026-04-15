
# =============================================================================
# 2. Clustering (no covariates)  –  Iris data
# =============================================================================

Y = iris[, 1:4]
l = iris[, 5]
K=3
# --- 2a. Mahalanobis distance ---
resMahC = PFC(Y, K = 3, distance = 'Mahalanobis',m=2,q=2)

# Cluster agreement
table(resMahC$l, l)
pairs(Y, col = resMahC$l)

u=resMahC$p # memberships 
d=resMahC$d # mahalanobis distances
c=resMahC$B # cluster centers

cov=resMahC$cov # covariance matrix
rho=resMahC$rho # how important cluster is
for(k in 1:K){ # scale cluster by importance
  cov[,,k]=cov[,,k]*rho[k]
}

# --- Compute XB ---
num <- 0
for(i in 1:nrow(resMahC$d)) {
  for(j in 1:ncol(resMahC$d)) {
    num <- num + (resMahC$p[i,j]^2) * (resMahC$d[i, j]^2)
  }
}

den <- Inf
u_mat <- as.matrix(u)

for (j in 1:(K - 1)) {
  for (l in (j + 1):K) {
    
    # centers of clusters j and l
    cj <- as.vector(c[1, , j])
    cl <- as.vector(c[1, , l])
    
    # fuzzy cluster sizes
    nj <- sum(u_mat[, j]) # size of cluster j
    nl <- sum(u_mat[, l]) # size of cluster k
    
    # pooled covariance
    Sigma_jl <- (nj * cov[, , j] + nl * cov[, , l]) / (nj + nl)
    
    # center difference vector
    diff <- matrix(cj - cl, ncol = 1) 
    
    # Mahalanobis distance-squared between centers
    sep <- as.numeric(t(diff) %*% solve(Sigma_jl) %*% diff)
    
    # keep the smallest one
    den <- min(den, sep)
  }
}

xb <- num/den

#numerator divide by the membership sum to the power of m (right now m = 2), raise distance to the power of q
#denominator - raise to the power of (q/2)
#make this into a function (centers, membership, covariance, distance, m, q)

