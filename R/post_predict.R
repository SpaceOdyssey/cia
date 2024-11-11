#' Draw from a posterior predictive distribution
#'
#' @description
#' Simulate samples from a posterior predictive distribution for a feature \eqn{f(g)}
#' a graph \eqn{g}.
#' 
#' @param x A cia_chain(s) object.
#' @param p_predict A function that draws from the posterior predictive distribution
#' of interest given an adjacency matrix representing a DAG. The function must 
#' be of the form p_predict(dag, ...) and return either a vector of numeric values.
#' @param ... Parameters to be passed to p_predict.
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
#' 
#' # Sample the edge probability.
#' SamplePosteriorPredictiveChains(dag_chains, function(dag) { return(dag) })
#' 
#' @returns A cia_post_chain(s) object.
#' 
#' @export
SamplePosteriorPredictiveChains <- function(x, p_predict, ...) UseMethod('SamplePosteriorPredictiveChains')

#' @export
SamplePosteriorPredictiveChains.cia_chain <- function(x, p_predict, ...) {
  
  args <- list(...)
  
  # Get dimensions of output.
  n <- length(x$state)
  args$dag <- x$state[[1]]
  n_pred <- length(do.call(p_predict, args))
  chain <- matrix(0.0, nrow = n, ncol = n_pred)

  # Sample from posterior predictive distribution.
  for (i in 1:n) {
    args$dag <- x$state[[i]]
    chain[i, ] <- do.call(p_predict, args)
  }

  return(new_cia_post_chain(chain))
}

#' @export
SamplePosteriorPredictiveChains.cia_chains <- function(x, p_predict, ...) {
  
  n_chains <- length(x)
  cl <- parallel::makeCluster(n_chains)
  doParallel::registerDoParallel(cl)
  
  i <- NULL
  chains <- foreach::foreach(i = 1:n_chains) %dopar% {
    SamplePosteriorPredictiveChains(x[[i]], p_predict, ...)
  }
  
  parallel::stopCluster(cl)
  
  return(new_cia_post_chains(chains))
}
