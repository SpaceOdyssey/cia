
#' @export
summary.cia_chains <- function(object, ...) {
  object <- object |>
    lapply(function(x) as.matrix(x$log_score))
  class(object) <- 'cia_post_chains'
  n_chains <- length(object)
  n_par <- 1
  stat_names <- c('mean', 'sd', 'n_eff')
  res <- list()
  for (i in 1:n_chains) {
    res[[i]] <- summary(object[[i]])
  }
  
  # Summarise total.
  stat_names <- c('Mean', 'SD', 'MCSE', 'S_eff', 'R_hat')
  res_mat <- matrix(nrow = n_par, ncol = length(stat_names),
                    dimnames = list(colnames(object), stat_names))
  
  flat <- FlattenChains(object)
  res_tot <- list()
  res_mat[, 1] <- colMeans(flat)
  res_mat[, 2] <- apply(flat, 2, stats::sd)
    res_mat[1, 4] <- object |> 
      lapply(function(x) as.vector(x)) |> 
      CalculateEffectiveSize.list()
    res_mat[1, 5] <- object |> 
      lapply(function(x) as.vector(x)) |> 
      CalculateSplitRHat()
  
  res_mat[, 3] <- res_mat[, 2]/sqrt(res_mat[, 4])
  
  res_tot$stats <- res_mat
  
  class(res_mat) <- 'summary.cia_chain'
  
  res[[n_chains + 1]] <- res_tot
  
  class(res) <- 'summary.cia_chains'
  
  return(res)
}

#' @export
summary.cia_chain <- function(object, ...) {
  object <- object |>
    lapply(function(x) as.matrix(x$log_score))
  stat_names <- c('Mean', 'SD', 'MCSE', 'N_eff')
  res_mat <- matrix(nrow = n_par, ncol = length(stat_names),
                    dimnames = list(colnames(object), stat_names))
  
  res_mat[, 1] <- colMeans(object)
  res_mat[, 2] <- apply(object, 2, stats::sd)
  res_mat[, 4] <- apply(object, 2, function(x) CalculateEffectiveSize.vector(x))
  res_mat[, 3] <- res_mat[, 2]/sqrt(res_mat[, 4])
  
  res <- list(stats = res_mat)
  
  class(res) <- 'summary.cia_chain'
  
  return(res)
}



#' @export
print.summary.cia_chains <- function(x, digits = 3, ...) {
  
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
print.summary.cia_chain <- function(x, digits = 3, ...) {
  print(x$stats, digits = digits)
}