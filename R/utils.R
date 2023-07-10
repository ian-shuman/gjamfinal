gelman_statistic <- function(chains, chain_vec){
 
  # Number of chains
  tot_m <- max(chain_vec)
  # Number of iterations per chain
  # Currently assumes even number of iterations in each chain
  n <- length(chain_vec) / tot_m
  
  # What chain to drop
  dropped <- sample(1:tot_m, 20, replace = T)
  dropped_2 <- sample(1:(tot_m-1), 20, replace = T)
  
  # Reformat chains into a matrix
  tot_chain_mat <- matrix(, nrow = n, ncol = tot_m)
  for(i in 1:tot_m){
    tot_chain_mat[,i] <- chains[which(chain_vec == i)]
  }
  
  R.hat <- c()
  
  for(k in 1:length(dropped)){
    chain_mat <- tot_chain_mat[,-dropped[k]]
    chain_mat <- chain_mat[,-dropped_2[k]]
    
    m <- tot_m - length(c(dropped[k], dropped_2[k]))
    
    # Mean coefficiente stimate of each chain
    theta.j.bar <- colMeans(chain_mat)
    
    # Mean coefficient estimate across chains
    theta.bar.bar <- (1 / m) * sum(theta.j.bar)
    
    # Across chain variance
    B <- (n / (m - 1)) * sum(theta.j.bar - theta.bar.bar)^2
    
    theta.diff <- matrix(, nrow = n, ncol = m)
    for(i in 1:n){
      theta.diff[i,] <- chain_mat[i,] - theta.j.bar
    }
    
    # Calculate within chain variance
    s.j.squared <- c()
    for(j in 1:m){
      s.j.squared[j] <- (1 / (n - 1)) * sum(theta.diff[,j])^2
    }
    
    # Within chain variance across chains
    W <- (1 / m) * sum(s.j.squared)
    
    # Weighted average
    var.theta.hat <- (1 - (1 / n)) * W + (1 / n) * B
    
    # Diagnostic statistic
    R.hat[k] <- sqrt(var.theta.hat / W)
    
    if(is.nan(R.hat[k])) R.hat[k] <- 0
  }
  return(R.hat = max(R.hat))
}
