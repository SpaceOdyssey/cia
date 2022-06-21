#' A transition object.

#' A one-step implementation of partition MCMC.
#' 
#' @examples 
#' PartitionMCMC(partitioned_nodes, proposal = ProposePartitionSplitJoin, scorer = scorer_1)
#' PartitionMCMC(partitioned_nodes, proposal = ProposePartitionSplitJoin, scorer = scorer_2)
#' 
#' @param current_state A labelled partition.
#' @param proposal A proposal function.
#' @param scorer A scorer object.
#'
#' @returns A new state.
#' 
#' @export
PartitionMCMC <- function(current_state, proposal, scorer) {
  
  proposed <- proposal(current_state$state)

  new_state <- proposed$state
  log_score_diff <- ScoreDiff(current_state$state, new_state, scorer)
  
  log_r <- log(proposed$current_nbd) - log(proposed$new_nbd) + log_score_diff
  
  accept <- AcceptProposal(log_r)
  mcmc_info <- list(accept = accept)
  
  if (accept) {
    current_state$state <- new_state
    current_state$log_score <- current_state$log_score + log_score_diff
    
    current_state$proposal_info <- proposed$proposal_info
    current_state$mcmc_info <- mcmc_info
  } else {
    current_state$proposal_info <- proposed$proposal_info
    current_state$mcmc_info <- mcmc_info
  }
  
  return(current_state)
}

#' Metropolis-Hastings acceptance.
#' 
#' @param log_r Log of Metropolis-Hastings ratio.
#' @return accept flag.
#' 
#' @noRd
AcceptProposal <- function(log_r) {
  log_p <- min(0.0, log_r)
  log_alpha <- log(runif(1))
  if (log_alpha < log_p) {
    accept <- TRUE
  } else {
    accept <- FALSE
  }
  
  return(accept)
}