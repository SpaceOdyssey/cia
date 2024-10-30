#' Sample edge probabilities
#' 
#' @param x A chain(s) or collection object where states are DAGs.
#' 
#' @returns p_edge A posterior sample for the marginalised edge probabilities.
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
#' pedge_sample <- SampleEdgeProbabilities(dag_chains)
#' 
#' @export
SampleEdgeProbabilities <- function(x) {
  p_edge <- SamplePosteriorPredictiveChains(x, function(dag) return(as.vector(dag)))
  
  return(p_edge)
} 
