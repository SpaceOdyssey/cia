#' Sample edge probabilities
#' 
#' @param x A chain(s) or collection object where states are DAGs.
#' 
#' @returns p_edge A posterior sample for the marginalised edge probabilities.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data
#'   )
#' 
#' init_state <- InitPartition(colnames(data), scorer)
#' results <- SampleChains(10, init_state, PartitionMCMC(), scorer)
#' dag_chains <- PartitiontoDAG(results, scorer)
#' 
#' pedge_sample <- SampleEdgeProbabilities(dag_chains)
#' 
#' @export
SampleEdgeProbabilities <- function(x) {
  p_edge <- SamplePosteriorPredictiveChains(x, function(dag) return(as.vector(dag)))
  
  return(p_edge)
} 
