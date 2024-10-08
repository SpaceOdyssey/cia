#' Calculate arithmetic mean for a DAG feature marginalised over DAGs.
#' 
#' @description
#' Calculate the posterior expected value for a feature (\eqn{f}, e.g., 
#' existence of an edge) whereby \deqn{E(f|D) = \sum_G p(f|G) p(G|D).} This can be 
#' useful for calculating point estimates of quantities of interests, such as 
#' the probability that an edge exists.
#' 
#' @param x A chain(s) or collection object.
#' @param p_feature A function that takes an adjacency matrix or collection object 
#' and returns a scalar corresponding to p(f|G). The function must be of the 
#' form p_feature(dag).
#' @param ... Extra parameters sent to the methods. For a dag collection you can
#' choose to use method='sampled' for MCMC sampled frequency (which is our 
#' recommended method) or method='score' which uses the normalised scores.
#' 
#' @returns p_post_feature A numeric value representing the posterior probability
#' of the feature.
#' 
#' @export
CalculateFeatureMean <- function(x, p_feature, ...) UseMethod('CalculateFeatureMean')

#' @export
CalculateFeatureMean.cia_chain <- function(x, p_feature, ...) {
  
  n <- length(x$state)
  p <- 0.0
  for (i in 1:n) { 
    p <- p + p_feature(x$state[[i]])
  }
  p <- p/n
  
  return(p)
}

#' @export
CalculateFeatureMean.cia_chains <- function(x, p_feature, ...) {
  
  n <- length(x)
  cl <- parallel::makeCluster(n)
  doParallel::registerDoParallel(cl)
  
  i <- NULL
  p <- foreach::foreach(i = 1:n) %dopar% {
    p[[i]] <- CalculateFeatureMean(x[[i]], p_feature, ...)
  }
  
  parallel::stopCluster(cl)
  
  return(p)
}

#' @export
CalculateFeatureMean.cia_collection <- function(x, p_feature, ...) {
  
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
CalculateFeatureMean.cia_collections <- function(x, p_feature, ...) {
  
  p <- list()
  for (i in 1:length(x)) {
    p[[i]] <- CalculateFeatureMean(x[[i]], p_feature, ...)
  }
  
  return(p)
}
