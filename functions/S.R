# compute variance matrix for spatially correlated random effects via Russel et al formula
S <- function(A, rho, dist) {
  
  # makes a p dimensional basis vector for coordinate
  make_basis <- function(k, p) replace(numeric(p), k, 1)
  
  storage <- list()
  
  for(i in 1:d) {
    
    N <- make_basis(i, 3) %*% t(make_basis(i, 3))
    
    O <- diag(L)
    
    for(j in 1:L) {
      
      for(k in 1:L) {
        
        O[j, k] <- exp(-dist[j, k]/rho[i])
        
      }
    }
    
    storage[[i]] <- kronecker(N, O)
    
  }
  
  V <- Reduce('+', storage)
  
  result <- kronecker(diag(L), A) %*% V %*% t(kronecker(diag(L), A))
  
  return(result)
  
}