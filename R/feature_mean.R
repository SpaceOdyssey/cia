#' Calculate arithmetic mean for a DAG feature
#' 
#' @description
#' Calculate the posterior expected value for a feature (\eqn{f(g)}, e.g., 
#' existence of an edge in graph \eqn{g}) by marginalising out the graph 
#' structure \eqn{q} over the graph space \eqn{G}, thus
#' \deqn{E(f|D) = \sum_{g \in G} f(g) p(g|D).} This can be 
#' useful for calculating point estimates of quantities of interests, such as 
#' the probability that an edge exists or the probability of one node being
#' an ancestor of another.
#' 
#' @param x A chain(s) or collection object.
#' @param p_feature A function that takes an adjacency matrix or collection object 
#' and returns a scalar corresponding to \eqn{f(g)}. The function must be of the 
#' form p_feature(dag).
#' @param ... Extra parameters sent to the methods. For a dag collection you can
#' choose to use estimated p(g|D) in two ways which can be specified using the 
#' 'method' parameter.method='sampled' for MCMC sampled frequency (which is our 
#' recommended method) or method='score' which uses the normalised scores.
#' 
#' @returns p_post_feature A numeric value representing the posterior probability
#' of the feature.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- DAGtoPartition(dag)
#' 
#' scorer <- list(
#'   scorer = BNLearnScorer, 
#'   parameters = list(data = data)
#'   )
#' 
#' results <- SampleChains(10, partitioned_nodes, PartitionMCMC(), scorer)
#' dag_chains <- PartitiontoDAG(results, scorer)
#' 
#' # Calculate the mean edge probability per chain.
#' CalculateFeatureMean(dag_chains, function(x) { return(x) })
#' 
#' # Calculate the mean edge probability across chains.
#' CalculateFeatureMean(FlattenChains(dag_chains), function(x) { return(x) })
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
    CalculateFeatureMean(x[[i]], p_feature, ...)
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
