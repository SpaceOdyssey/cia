# Summary function for a cia_chain or cia_chains object.

#' @export
summary.cia_chains <- function(object, 
                               stat_names = c('Mean', 'SD', 'MCSE', 'R_hat', 'N_eff'),
                               ...) {
  
  object <- object |>
    lapply(function(x) as.matrix(x$log_score))
  class(object) <- 'cia_post_chains'
  
  res <- summary(object, stat_names = stat_names)
  
  return(res)
}

#' @export
summary.cia_chain <- function(object, 
                              stat_names = c('Mean', 'SD', 'MCSE', 'R_hat', 'N_eff'),
                              ...) {

  object <- matrix(object$log_score)
  class(object) <- 'cia_post_chain'
  res <- summary(object, stat_names = stat_names)
  
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