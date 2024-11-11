#' Get the maximum a posteriori state
#' 
#' @param x A collection of unique objects or chains object.
#' 
#' @returns A list with the adjacency matrix for the map and it's posterior 
#' probability. It is possible for it to return multiple DAGs. The list has
#' elements;
#' \itemize{
#'  \item state: List of MAP DAGs.
#'  \item log_p: Numeric vector with the log posterior probability for each state.
#'  \item log_state_score: Numeric vector representing the log score for each state.
#'  \item log_norm_state_score: Numeric vector representing the log of the 
#'  normalised score for each state.
#' }
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
#' 
#' # Get the MAP per chain. Can be helpful to compare chains.
#' GetMAP(results)
#' 
#' # Get MAP across all chains.
#' results |>
#'   FlattenChains() |>
#'   GetMAP()
#' 
#' 
#' @export
GetMAP <- function(x) UseMethod('GetMAP')

#' @export
GetMAP.cia_collections <- function(x) {
  
  n_chains <- length(x)
  maps <- list()
  for (i in 1:n_chains) {
    maps[[i]] <- GetMAP(x[[i]])
  }
  
  return(maps)
}

#' @export
GetMAP.cia_collection <- function(x) {
  
  p_maps <- max(x$log_norm_state_score)
  ip_map <- which(x$log_norm_state_score == p_maps)
  
  maps <- list(
    state = x$state[ip_map],
    log_p = exp(p_maps),
    log_state_score = x$log_state_score[ip_map],
    log_norm_state_score = x$log_norm_state_score[ip_map]
  )
  
  return(maps)
}

#' @export
GetMAP.cia_chains <- function(x) {
  
  collection <- CollectUniqueObjects(x)
  
  return(GetMAP(collection))
}

#' @export
GetMAP.cia_chain <- function(x) {
  
  collection <- CollectUniqueObjects(x)
  
  return(GetMAP(collection))
}

