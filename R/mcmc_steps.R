#' A transition object.

#' A one-step implementation of partition MCMC. This acts as a constructor.
#' 
#' @examples 
#' transition <- PartitionMCMC(proposal = ProposePartitionSplitJoin)
#' transition(current_state, scorer_1)
#' 
#' @param proposal Proposal function. Default is the DefaultProposal.
#' @param verbose Flag to pass mcmc information.
#' @param current_state A labelled partition.
#' @param scorer Scorer object.
#'
#' @returns A new state.
#' 
#' @export
PartitionMCMC <- function(proposal = DefaultProposal(), verbose = TRUE) {
  
  function(current_state, scorer) {
    proposed <- proposal(current_state$state)
  
    new_state <- proposed$state
    log_score_diff <- ScoreDiff(current_state$state, new_state, scorer)
    
    log_r <- log(proposed$current_nbd) - log(proposed$new_nbd) + log_score_diff
    
    accept <- AcceptProposal(log_r)
    
    if (accept) {
      current_state$state <- new_state
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
  log_alpha <- log(runif(1))
  if (log_alpha < log_p) {
    accept <- TRUE
  } else {
    accept <- FALSE
  }
  
  return(accept)
}