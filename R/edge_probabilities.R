#' Calculate pairwise edge probabilities
#' 
#' Calculate pairwise edge probabilities. The posterior probability of an edge 
#' \eqn{E} given the data \eqn{D} is given by marginalising out 
#' the graph structure \eqn{g} over the graph space \eqn{G}, such that 
#' \deqn{p(E|D) = \sum_{g \in G} p(E|g)p(g|D).}
#' 
#' The posterior probability for a given graph p(g|D) is estimated in two
#' ways which can be specified using the 'method' parameter.
#' 
#' @param x A cia_chain(s) or collection object where states are DAGs.
#' @param ... Extra parameters sent to the methods. For a dag collection you can
#' choose to use estimated p(g|D) in two ways which can be specified using the 
#' 'method' parameter.method='sampled' for MCMC sampled frequency (which is our 
#' recommended method) or method='score' which uses the normalised scores.
#' 
#' @returns Matrix of edge probabilities.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- DAGtoPartition(dag)
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data
#'   )
#' 
#' results <- SampleChains(10, partitioned_nodes, PartitionMCMC(), scorer)
#' dag_chains <- PartitiontoDAG(results, scorer)
#' CalculateEdgeProbabilities(dag_chains)
#' 
#' @export
CalculateEdgeProbabilities <- function(x, ...) UseMethod('CalculateEdgeProbabilities')

#' @export
CalculateEdgeProbabilities.cia_chain <- function(x, ...) {
  
  p_edge <- x$state |>
    simplify2array() |>
    apply(c(1, 2), mean)
  
  return(p_edge)
}

#' @export
CalculateEdgeProbabilities.cia_chains <- function(x, ...) {
  
  p_edge <- list()
  for (i in 1:length(x)) {
    p_edge[[i]] <- CalculateEdgeProbabilities(x[[i]])
  }
  
  return(p_edge)
}

#' @export
CalculateEdgeProbabilities.cia_collection <- function(x, method = 'sampled', ...) {
  
  y <- list(...)
  if (!'method' %in% names(y))
    y$method <- 'sampled'
  
  stopifnot(y$method %in% c('sampled', 'score'))
  
  if (y$method == 'sampled') {
    p <- exp(x$log_sampling_prob)
  } else if (y$method == 'score') {
    p <- exp(x$log_norm_state_score)
  }
  
  names <- colnames(x$state[[1]])
  n <- length(names)
  p_edge <- matrix(0.0, nrow = n, ncol = n, dimnames = list(names, names))
  for (i in 1:length(x$state)) {
    p_edge <- p_edge + p[i]*x$state[[i]]
  }
  
  return(p_edge)
}

#' @export
CalculateEdgeProbabilities.cia_collections <- function(x, ...) {
  
  p_edge <- list()
  for (i in 1:length(x)) {
    p_edge[[i]] <- CalculateEdgeProbabilities(x[[i]], ...)
  }
  
  return(p_edge)
}
