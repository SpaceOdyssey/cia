# Chain objects.

#' Constructor for a single chain.
#' 
#' @param x A list corresponding to a single mcmc chain.
#' @returns cia_chain A cia_chain object.
#' 
#' @noRd
new_cia_chain <- function(x = list()) {
  
  stopifnot(is.list(x))
  cia_chain <- structure(x, class = 'cia_chain')
  
  return(cia_chain)
}

#' Indexing with respect to iterations.
#' 
#' @param x A cia_chain object.
#' @param i An index.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_chain.
#' 
#' @export
`[.cia_chain` <- function(x = list(), i, ...) {
  
  class(x) <- 'list'
  
  chain <- list()
  chain$state <- x$state[i, ...]
  chain$log_score <- x$log_score[i, ...]
  chain$proposal_info <- x$proposal_info[i, ...]
  chain$mcmc_info <- x$mcmc_info[i, ...]
  
  chain <- new_cia_chain(chain)
  
  return(chain)
}

#' Constructor for more than one chain.
#' 
#' @param x A list corresponding to more than one mcmc chain.
#' @returns cia_chains A cia_chains object.
#' 
#' @noRd
new_cia_chains <- function(x = list()) {
  
  stopifnot(is.list(x))
  cia_chains <- structure(x, class = 'cia_chains')
  
  return(cia_chains)
}

#' Index a cia_chains object.
#' 
#' @param x A cia_chains object.
#' @param i An index to get the cia_chain.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_chains object. 
#'
#' @export
`[[.cia_chains` <- function(x, i, ...) {
  
  class(x) <- 'list'
  return(new_cia_chain(x[[i, ...]]))
}

#' Index a cia_chains object with respect to iterations.
#' 
#' @param x A cia_chain object.
#' @param i An index to get the cia_chain iterations.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_chains object. 
#' 
#' @export
`[.cia_chains` <- function(x = list(), i, ...) {
  
  class(x) <- 'list'
  
  n_chains <- length(x)
  chains <- list()
  for (j in 1:n_chains) {
    chains[[j]] <- x[[j]][i, ...]
  }
  
  chains <- new_cia_chains(chains)
  
  return(chains)
}
