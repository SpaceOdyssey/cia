# Calculate convergence statistics.
# TODO: Use FFT auto-correlation estimation. 
# TODO: Incorporate rank-normalised approach.

#' Split R-hat calculation.
#' 
#' @description
#' Calculates the Split R-hat as proposed by Vehtari et al. (2021) which 
#' overcomes some problems with the classical R-hat calculation (Gelman & Rubin, 
#' 1992).
#' 
#' @param x A list of vectors or a vector representing an MCMC chain(s) in 
#' equilibrium for a parameter.
#' 
#' @return r_hat A scalar.
#' 
#' @references
#' Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., and 
#' Rubin, D. B. (2013). Bayesian Data Analysis, third edition. CRC Press. 
#' MR3235677. 668, 669, 670, 672, 673, 675, 676, 687
#' 
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C. (2021). 
#' Rank-normalization, folding, and localization: An improved R-hat for assessing 
#' convergence of MCMC (with discussion). Bayesian analysis, 16(2), 667-718.
#' Gelman, A. and Rubin, D. B. (1992) Inference from iterative simulation using 
#' multiple sequences, Statistical Science, 7, 457-511.
#' 
#' @noRd
CalculateSplitRHat <- function(x) UseMethod('CalculateSplitRHat')

#' @noRd
CalculateSplitRHat.numeric <- function(x) {
  return(CalculateSplitRHat(list(x)))
}

#' @noRd
CalculateSplitRHat.list <- function(x) {
  
  theta <- SplitChains(x)
  
  N <- GetN(theta)
  B <- CalculateB(theta)
  W <- CalculateW(theta)
  
  var_plus <- CalculateVarPlus(W, B, N)
  
  r_hat <- sqrt(var_plus/W)
  
  return(r_hat)
}

#' Calculate the effective sample size.
#' 
#' @description
#' Calculate effective sample size in accordance with the recommendation
#' in Vehtari et al. (2021).
#' 
#' @param x A vector representing a single chain or list of vectors representing 
#' multiple MCMC chains.
#' 
#' @return n_eff A scalar representing the approximate number of effective 
#' samples.
#' 
#' @noRd
CalculateEffectiveSize <- function(x) UseMethod('EffectiveSize')

#' @noRd
CalculateEffectiveSize.list <- function(x) {
  
  theta <- SplitChains.list(x)
  s_eff <- EffectiveSizeGivenSplitChains(theta)

  return(s_eff)
}

#' @noRd
CalculateEffectiveSize.numeric <- function(x) {
  
  theta <- SplitChains.numeric(x)
  n_eff <- EffectiveSizeGivenSplitChains(theta)
  
  return(n_eff)
}

#' @noRd
EffectiveSizeGivenSplitChains <- function(theta) {
  
  M <- GetM(theta)
  N <- GetN(theta)
  
  B <- CalculateB(theta)
  W <- CalculateW(theta)
  
  ssq <- CalculateChainVariances(theta)
  W <- CalculateW(theta)
  
  var_plus <- CalculateVarPlus(W, B, N)
  
  # Using the acf recommendation for maximum lags. There are other more
  # sophisticated implementations that could be used as outlined by
  # Vehtari et al. (2021).
  lag_max <- as.integer(10*log10(N))
  ssq_chain_rhos <- matrix(nrow = M, ncol = lag_max + 1)
  for (m in 1:M) {
    ssq_chain_rhos[m, ] <- ssq[m]*stats::acf(theta[[m]], lag.max = lag_max, plot = FALSE)$acf
  }
  
  rho <- 1.0 - (W - colMeans(ssq_chain_rhos))/var_plus
  
  n_p_hat <- length(rho)/2
  p_hat <- vector('numeric', length = n_p_hat)
  for (t in 1:n_p_hat) {
    p_hat[t] <- rho[1 + 2*(t - 1)] + rho[2 + 2*(t - 1)]
  }
  
  # Truncate to enforce autocorrelations are positive.
  p_hat_pos <- which(p_hat <= 0.0)
  k <- ifelse(any(p_hat_pos), p_hat_pos[1], n_p_hat)
  
  tau <- -1.0 + 2*sum(p_hat[1:k])
  
  s_eff <- N*M/tau
  
  return(s_eff)
}

# Helper functions.

#' Split chains.
#' 
#' @noRd
SplitChains <- function(x) UseMethod('SplitChains')

#' @noRd
SplitChains.list <- function(x) {
  
  # Split simulated chains.
  M_chains <- length(x)
  N_tot <- length(x[[1]])
  
  # Remove first observation from chains if odd number.
  if (N_tot %% 2) {
    for (i in 1:M_chains) { x[[i]] <- x[2:N_tot] }
    N_tot <- N_tot - 1
  }
  
  N <- N_tot %/% 2
  
  theta <- list()
  for (m in 1:M_chains) {
    theta[[1 + 2*(m - 1)]] <- x[[m]][1:N]
    theta[[2 + 2*(m - 1)]] <- x[[m]][(N + 1):N_tot]
  }
  
  return(theta)
}

#' @noRd
SplitChains.numeric <- function(x) {
  
  N_tot <- length(x)
  
  # Remove first observation from chains if odd number.
  if (N_tot %% 2) {
    N_tot <- N_tot - 1
    x <- x[1:N_tot]
  }
  
  N <- N_tot %/% 2
  
  x <- list(x[1:N], x[(N + 1):N_tot])
  
  return(x)
}

#' Calculate between chain variance.
#' 
#' @param x vector
#' @return B The between chain variance of the means.
#' 
#' @noRd
CalculateB <- function(x) {
  
  N <- GetN(x)
  M <- GetM(x)
  
  chain_means <- CalculateChainMeans(x)
  tot_mean <- mean(chain_means)
  
  B <- N/(M - 1)*sum((chain_means - tot_mean)^2)
  
  return(B)
}

#' Calculate within chain variance.
#'
#' @param x A list of vectors.
#' @return W The within chain variance averaged across chains.
#' 
#' @noRd
CalculateW <- function(x) {
  
  ssq <- CalculateChainVariances(x)
  W <- mean(ssq)
  
  return(W)
}

#' @noRd
CalculateChainVariances <- function(x) {
  
  N <- GetN(x)
  M <- GetM(x)
  
  chain_means <- CalculateChainMeans(x)
  
  # Calculate within chain contribution.
  ssq <- vector('numeric', M)
  for (m in 1:M) { 
    ssq[m] <- 1/(N - 1)*sum((x[[m]] - chain_means[m])^2) 
  }
  
  return(ssq)
}

#' @noRd
CalculateChainMeans <- function(x) {
  
  means <- x |>
    lapply(mean) |>
    unlist()
  
  return(means)
}

#' @noRd
CalculateVarPlus <- function(W, B, N) {
  return((N - 1)*W/N + B/N)
}

#' @noRd
GetM <- function(x) {
  return(length(x))
}

#' @noRd
GetN <- function(x) {
  return(length(x[[1]]))
}
