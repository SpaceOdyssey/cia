#' Flatten chains
#' 
#' @description
#' Flatten a cia_chains object into a single cia_chain object. This is helpful
#' for when you want to calculate a feature across using all samples across
#' the cia_chains.
#' 
#' @param chains A cia_chains object.
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
#' FlattenChains(results)[1:3]
#' 
#' @export
FlattenChains <- function(chains) UseMethod('FlattenChains')

#' @export
FlattenChains.cia_chains <- function(chains) {
  
  n_chains <- length(chains)
  chain <- list()
  
  chain$state <- list()
  chain$log_score <- c()
  chain$proposal_info <- list()
  chain$mcmc_info <- list()
  for (i in 1:n_chains) {
    chain$state <- c(chain$state, chains[[i]]$state)
    chain$log_score <- c(chain$log_score, chains[[i]]$log_score)
    chain$proposal_info <- c(chain$proposal_info, chains[[i]]$proposal_info)
    chain$mcmc_info <- c(chain$mcmc_info, chains[[i]]$mcmc_info)
  }
  
  chain <- new_cia_chain(chain)
  
  return(chain)
}

#' @export
FlattenChains.cia_post_chains <- function(chains) {
  
  chain <- chains |>
    do.call(rbind, args = _) |>
    new_cia_post_chain()
  
  return(chain)
}

