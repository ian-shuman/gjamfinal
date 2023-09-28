gelman_rubin <- function(par_matrix, chains, samples){
  
  # Coerce to matrix
  par_matrix <- as.data.frame(par_matrix)
  
  # Mean over all samples
  all_mean <- mean(par_matrix[,2])
  
  # Mean of each chain
  chain_mean <- apply(par_matrix, 2, mean)
  
  # Variance of each chain
  chain_var <- apply(par_matrix, 2, stats::var)
  W <- (1 / chains) * sum(chain_var)
  B <- samples / (chains - 1) * sum((chain_mean - all_mean)^2)
  V <- (1 - 1 / samples) * W + (1 / samples) * B
  round(sqrt(V / W), 4)
}

gelman_statistic <- function(chains, chain_vec){
 
  # Number of chains
  tot_m <- max(chain_vec)
  # Number of iterations per chain
  # Currently assumes even number of iterations in each chain
  n <- length(chain_vec) / tot_m
  
  chain_ind <- matrix(, nrow = tot_m, ncol = 2)
  
  for(i in 1:tot_m){
    if(i == 1){
      chain_ind[i,] <- c(1, n)
    }else{
      chain_ind[i,] <- c(1+chain_ind[i-1,2],n+chain_ind[i-1,2])
    }
  }
  
  index_i <- 1:tot_m
  index_j <- 1:tot_m
    
  same_chain <- matrix(, nrow = 1, ncol = 2)
  
  for(i in index_i){
    for(j in index_j){
      if(i == j) break
      chain_i <- chains[chain_ind[i,1]:chain_ind[i,2]]
      chain_j <- chains[chain_ind[j,1]:chain_ind[j,2]]
      if(all(chain_i == chain_j)) same_chain <- rbind(same_chain, c(i, j))
    }
  }
  
  same_chain <- matrix(same_chain[-1,], ncol = 2)
  
  keep_chain <- same_chain[1,1]
  keep_chain <- c(keep_chain, ifelse(nrow(same_chain) > 1, ifelse(sum(same_chain[1,]) == sum(same_chain[2,]), NA, same_chain[2,1]), NA))
  keep_chain <- c(keep_chain, ifelse(!any(!(1:tot_m %in% same_chain)), NA, max(0, which(!(1:tot_m %in% same_chain)))))
  keep_chain <- na.omit(keep_chain)
  
  m <- length(keep_chain)
  
  # Reformat chains into a matrix
  tot_chain_mat <- matrix(, nrow = n, ncol = m)
  ind <- 0
  for(i in keep_chain){
    ind <- ind + 1
    tot_chain_mat[,ind] <- chains[which(chain_vec == i)]
  }
  
  # Mean coefficient estimate for each chain
  theta.j.bar <- colMeans(tot_chain_mat)
  
  # Mean coefficient estimateacross chains
  theta.bar.bar <- (1 / m) * sum(theta.j.bar)
  
  # Across chain variance
  B <- (n / (m - 1)) * sum(theta.j.bar - theta.bar.bar)^2
  
  theta.diff <- matrix(, nrow = n, ncol = m)
  for(i in 1:n){
    theta.diff[i,] <- tot_chain_mat[i,] - theta.j.bar
  }
  
  # Calculate within chain variances
  s.j.squared <- c()
  for(j in 1:m){
    s.j.squared[j] <- (1 / (n - 1)) * sum(theta.diff[,j])^2
  }
  
  # Within chain variance across chains
  W <- (1 / m) * sum(s.j.squared)
  
  # Weighted average
  var.theta.hat <- (1 - (1 / n)) * W + (1 / n) * B
  
  # Diagnostic statistic
  R.hat <- sqrt(var.theta.hat / W)
  
  test_R.hat <- gelman_rubin(par_matrix = tot_chain_mat, chains = m, samples = n)
  
  return(R.hat = test_R.hat)
}
