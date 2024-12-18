# Summarise posterior predictive chains.

#' @export
summary.cia_post_chains <- function(object, 
                                    stat_names = c('Mean', 'SD', 'MCSE', 'R_hat', 'N_eff'),
                                    ...) {
  
  n_chains <- length(object)
  n_par <- ncol(object[[1]])
  res <- list()
  for (i in 1:n_chains) {
    res[[i]] <- summary(object[[i]])
  }
  
  # Summarise total.
  n_stats <- length(stat_names)
  res_mat <- matrix(nrow = n_par, ncol = n_stats,
                    dimnames = list(colnames(object[[1]]), stat_names))
  
  flat <- FlattenChains(object)
  res_tot <- list()
  for (i in 1:n_stats) {
    stat_name <- casefold(stat_names[i])
    
    if (stat_name == 'mean')
      res_mat[, i] <- colMeans(flat)
    if (stat_name == 'median')
      res_mat[, i] <- apply(flat, 2, median)
    if (stat_name == 'sd')
      res_mat[, i] <- apply(flat, 2, stats::sd)
    if (stat_name == 'mcse') {
      sd <- apply(flat, 2, stats::sd)
      n_eff <- apply(flat, 2, function(x) CalculateEffectiveSize.numeric(x))
      res_mat[, i] <- sd/sqrt(n_eff)
    } 
    if (startsWith(stat_name, 'q')) {
      q <- stat_name |> stringr::str_remove('q') |> as.numeric()
      res_mat[, i] <- apply(flat, 2, function(x) stats::quantile(x, probs = q/100))
    }
    if (stat_name == 'r_hat') {
      if (n_par == 1) {
        res_mat[1, i] <- object |> 
          lapply(function(x) as.vector(x)) |> 
          CalculateSplitRHat.list()
      } else {
        for (j in 1:n_par) {
          res_mat[j, i] <- object |> 
            lapply(function(x) as.vector(x[, j])) |> 
            CalculateSplitRHat.list()
        }
      }
    }
    if (stat_name == 'n_eff') {
      if (n_par == 1) {
        
        res_mat[1, i] <- object |> 
          lapply(function(x) as.vector(x)) |> 
          CalculateEffectiveSize.list()
      } else {
        for (j in 1:n_par) {
          res_mat[j, i] <- object |> 
            lapply(function(x) as.vector(x[, j])) |> 
            CalculateEffectiveSize.list()
        }
      }
    }
  }
  
  res_tot$stats <- res_mat
  
  class(res_mat) <- 'summary.cia_post_chain'
  
  res[[n_chains + 1]] <- res_tot
  
  class(res) <- 'summary.cia_post_chains'
  
  return(res)
}

#' @export
print.summary.cia_post_chains <- function(x, digits = 3, ...) {
  
  n_chains <- length(x) - 1
  for (i in 1:n_chains) {
    cat('Chain:', i, '\n')
    print(x[[i]], digits = digits)
    cat('\n')
  }
  
  # Summarise total.
  cat('Overall:\n')
  print(x[[n_chains + 1]], digits = digits)
  cat('\n')
}

#' @export
summary.cia_post_chain <- function(object, 
                                   stat_names = c('Mean', 'SD', 'MCSE', 'R_hat', 'N_eff'),
                                   ...) {
  
  n_stats <- length(stat_names)
  res_mat <- matrix(nrow = ncol(object), ncol = n_stats,
                    dimnames = list(colnames(object), stat_names))
  
  for (i in 1:n_stats) {
    stat_name <- casefold(stat_names[i])
    
    if (stat_name == 'mean')
      res_mat[, i] <- colMeans(object)
    if (stat_name == 'median')
      res_mat[, i] <- apply(object, 2, stats::median)
    if (stat_name == 'sd')
      res_mat[, i] <- apply(object, 2, stats::sd)
    if (stat_name == 'r_hat')
      res_mat[, i] <- apply(object, 2, function(x) CalculateSplitRHat.numeric(x))
    if (stat_name == 'n_eff')
      res_mat[, i] <- apply(object, 2, function(x) CalculateEffectiveSize.numeric(x))
    if (stat_name == 'mcse') {
      std <- apply(object, 2, stats::sd)
      n_eff <- CalculateEffectiveSize.numeric(object)
      res_mat[, i] <- std/sqrt(n_eff)
    }
    if (startsWith(stat_name, 'q')) {
      q <- stat_name |> stringr::str_remove('q') |> as.numeric(q)
      res_mat[, i] <- apply(object, 2, function(x) stats::quantile(x, probs = q/100))
    }
  }
  
  res <- list(stats = res_mat)
  
  class(res) <- 'summary.cia_post_chain'
  
  return(res)
}

#' @export
print.summary.cia_post_chain <- function(x, digits = 3, ...) {
  print(x$stats, digits = digits)
}
