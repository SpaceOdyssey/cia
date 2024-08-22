# Posterior prediction chains.

#' Constructor for a single posterior predictive chain.
#' 
#' @param x A data.frame corresponding to a single mcmc chain.
#' 
#' @noRd
new_cia_post_chain <- function(x) {
  
  cia_post_chain <- structure(x, class = 'cia_post_chain')
  
  return(cia_post_chain)
}

#' Indexing with respect to iterations.
#' 
#' @param x A cia_post_chain object.
#' @param i An index.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_post_chain.
#' 
#' @export
`[.cia_post_chain` <- function(x = list(), i, ...) {
  
  x <- as.matrix(x)
  class(x) <- 'matrix'
  chain <- x[i, ...]
  chain <- new_cia_post_chain(chain)
  
  return(chain)
}

#' Constructor for more than one chain.
#' 
#' @param x A list corresponding to more than one cia_post_chain objects.
#' @returns cia_post_chains A cia_post_chains object.
#' 
#' @noRd
new_cia_post_chains <- function(x = list()) {
  
  stopifnot(is.list(x))
  cia_post_chains <- structure(x, class = 'cia_post_chains')
  
  return(cia_post_chains)
}

#' Index a cia_post_chains object.
#' 
#' @param x A cia_post_chains object.
#' @param i An index to get the cia_post_chain.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_post_chains object. 
#'
#' @export
`[[.cia_post_chains` <- function(x, i, ...) {
  
  class(x) <- 'list'
  return(new_cia_post_chain(x[[i, ...]]))
}

#' Index a cia_post_chains object with respect to iterations.
#' 
#' @param x A cia_post_chain object.
#' @param i An index to get the cia_post_chain iterations.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_post_chains object. 
#' 
#' @export
`[.cia_post_chains` <- function(x = list(), i, ...) {
  
  class(x) <- 'list'
  
  n_chains <- length(x)
  chains <- list()
  for (j in 1:n_chains) {
    chains[[j]] <- x[[j]][i, ...]
  }
  
  chains <- new_cia_post_chains(chains)
  
  return(chains)
}
