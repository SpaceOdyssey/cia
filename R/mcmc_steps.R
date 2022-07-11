#' Transition objects.

#' A one step implementation of partition MCMC. This acts as a constructor.
#' 
#' @description 
#' This is a constructor for a single Partition MCMC step. The function
#' constructs an environment with the proposal and verbose flag. It then returns
#' a function which takes the current_state and a scorer object.
#' 
#' @examples
#' dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' 
#' scorer <- list(
#'   scorer = BNLearnScorer,
#'   parameters = list(data = bnlearn::learning.test)
#'   )
#' 
#' current_state <- list(
#'   state = partitioned_nodes,
#'   log_score = ScoreLabelledPartition(partitioned_nodes, scorer)
#'   )
#' 
#' pmcmc <- PartitionMCMC(proposal = PartitionSplitJoin)
#' pmcmc(current_state, scorer)
#' 
#' @param proposal Proposal function. Default is the DefaultProposal.
#' @param verbose Flag to pass mcmc information.
#'
#' @returns Function that takes the current state and scorer that outputs a new
#' state.
#' 
#' @export
PartitionMCMC <- function(proposal = NULL, verbose = TRUE) {
  
  if (is.null(proposal))
    proposal <- DefaultProposal()
  
  function(current_state, scorer) {
    proposed <- proposal(current_state$state)
    
    log_score_diff <- ScoreDiff(current_state$state, proposed$state, scorer)
    log_r <- log(proposed$current_nbd) - log(proposed$new_nbd) + log_score_diff
    
    accept <- AcceptProposal(log_r)
    if (accept) {
      current_state$state <- proposed$state
      current_state$log_score <- current_state$log_score + log_score_diff
      current_state$proposal_info <- proposed$proposal_info
    } else {
      current_state$proposal_info <- proposed$proposal_info
    }
    
    if (verbose)
      current_state$mcmc_info <- list(accept = accept)
    
    return(current_state)
  }
}

#' Metropolis-Hastings acceptance.
#' 
#' @param log_r Log of Metropolis-Hastings ratio.
#' @return accept flag.
#' 
#' @noRd
AcceptProposal <- function(log_r) {
  log_p <- min(0.0, log_r)
  log_alpha <- log(stats::runif(1))
  if (log_alpha < log_p) {
    accept <- TRUE
  } else {
    accept <- FALSE
  }
  
  return(accept)
}
