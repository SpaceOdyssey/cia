# Chain objects.

#' Constructor for a single chain.
#' 
#' @param x A list corresponding to a single mcmc chain.
#' @returns cia_chain A cia_chain object.
#' 
#' @noRd
new_cia_chain <- function(x = list()) {
  
  stopifnot(is.list(x))
  cia_chain <- structure(x, class = 'cia_chain')
  
  return(cia_chain)
}

#' Index a cia_chain object.
#' 
#' @param x A cia_chain object.
#' @param i An index.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_chain.
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
#' results[[1]][5]
#' 
#' @export
`[.cia_chain` <- function(x = list(), i, ...) {
  
  class(x) <- 'list'
  
  chain <- list()
  chain$state <- x$state[i, ...]
  chain$log_score <- x$log_score[i, ...]
  chain$proposal_info <- x$proposal_info[i, ...]
  chain$mcmc_info <- x$mcmc_info[i, ...]
  
  chain <- new_cia_chain(chain)
  
  return(chain)
}

#' Constructor for more than one chain.
#' 
#' @param x A list corresponding to more than one cia_chain.
#' @returns cia_chains A cia_chains object.
#' 
#' @noRd
new_cia_chains <- function(x = list()) {
  
  stopifnot(is.list(x))
  cia_chains <- structure(x, class = 'cia_chains')
  
  return(cia_chains)
}

#' Index a cia_chains object.
#' 
#' @param x A cia_chains object.
#' @param i An index to get the cia_chain.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_chains object. 
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
#' results[[1]][1:3]
#' 
#' @export
`[[.cia_chains` <- function(x, i, ...) {
  
  class(x) <- 'list'
  return(new_cia_chain(x[[i, ...]]))
}

#' Index a cia_chains object
#' 
#' @param x A cia_chain object.
#' @param i An index to get the cia_chain iterations.
#' @param ... ellipsis for extra indexing parameters.
#'
#' @returns chain A cia_chains object. 
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
#' results[5]
#' 
#' @export
`[.cia_chains` <- function(x = list(), i, ...) {
  
  class(x) <- 'list'
  
  n_chains <- length(x)
  chains <- list()
  for (j in 1:n_chains) {
    chains[[j]] <- x[[j]][i, ...]
  }
  
  chains <- new_cia_chains(chains)
  
  return(chains)
}
