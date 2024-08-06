#' Collect feature probability marginalised over states.
#' 
#' @description
#' Calculate the feature (\eqn{f}) probability whereby 
#' \eqn{p(f|D) = \sum_\mathcal{G \in G} p(G|D)p(f|G)}.
#' 
#' @param x A chain(s) or collection object.
#' @param p_feature A function that takes an adjacency matrix and collection object 
#' and returns a numeric value equal to p(f|G). Therefore, it must be of the 
#' form p_feature(dag).
#' @param ... Extra parameters sent to the methods. For a dag collection you can
#' choose to use method='sampled' for MCMC sampled frequency (which is our 
#' recommended method) or method='score' which uses the normalised scores.
#' 
#' @returns p_post_feature A numeric value representing the posterior probability
#' of the feature.
#' 
#' @export
CalculateFeatureProbability <- function(x, p_feature, ...) UseMethod('CalculateFeatureProbability')

#' @export
CalculateFeatureProbability.cia_chain <- function(x, p_feature, ...) {
  
  n <- length(x$state)
  p <- 0.0
  for (i in 1:n) { p <- p + p_feature(x$state) }
  p <- p/n
  
  return(p)
}

#' @export
CalculateFeatureProbability.cia_chains <- function(x, p_feature, ...) {
  
  p <- list()
  for (i in 1:length(x)) {
    p[[i]] <- CalculateEdgeProbabilities(x[[i]])
  }
  
  return(p)
}

#' @export
CalculateFeatureProbability.cia_collection <- function(x, p_feature, ...) {
  
  y <- list(...)
  if (!'method' %in% names(y))
    y$method <- 'sampled'
  
  stopifnot(y$method %in% c('sampled', 'score'))
  
  if (y$method == 'sampled') {
    p_state <- exp(x$log_sampling_prob)
  } else if (y$method == 'score') {
    p_state <- exp(x$log_norm_state_score)
  }

  n <- length(x$state)
  p <- 0.0
  for (i in 1:n) { p <- p + p_state[i]*p_feature(x$state[[i]]) }
  
  return(p)
}

#' @export
CalculateFeatureProbability.cia_collections <- function(x, p_feature, ...) {
  
  p <- list()
  for (i in 1:length(x)) {
    p[[i]] <- CalculateFeatureProbability(x[[i]], ...)
  }
  
  return(p)
}
