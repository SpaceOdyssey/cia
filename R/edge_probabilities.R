#' Calculate marginalised edge probabilities.
#' 
#' Calculate the probability of a given edge (\eqn{E}) given the data which
#' is given by, \deqn{p(E|D) = \sum_\mathcal{G} p(E|G)p(G|D)}).
#' 
#' @param x A chain(s) or collection object where states are DAGs. See CollectUniqueObjects.
#' @param ... Extra parameters sent to the methods. For a dag collection you can
#' choose to use method='sampled' for MCMC sampled frequency (which is our 
#' recommended method) or method='score' which uses the normalised scores.
#' 
#' @returns p_edge An adjacency matrix representing the edge probabilities.
#' 
#' @export
CalculateEdgeProbabilities <- function(x, ...) UseMethod('CalculateEdgeProbabilities')

CalculateEdgeProbabilities.dagmc_chain <- function(x) {
  
  p_edge <- x$state |>
    simplify2array() |>
    apply(c(1, 2), mean)
  
  return(p_edge)
}

CalculateEdgeProbabilities.dagmc_chains <- function(chains) {
  
  p_edge <- list()
  for (i in 1:length(chains)) {
    p_edge[[i]] <- CalculateEdgeProbabilities(chains[[i]])
  }
  
  return(p_edge)
}
