#' Transition objects.

#' One step implementation of the tempered partition MCMC. This acts as a 
#' constructor.
#' 
#' @description 
#' This is a constructor for a single Tempered Partition MCMC step. The function
#' constructs an environment with the proposal, inverse temperature, and verbose 
#' flag. It then returns a function which takes the current_state and a scorer 
#' object. This only allows the scores to be raised to a constant temperature
#' for every step.
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
#' pmcmc <- PartitionMCMC(proposal = NodeMove, temperature = 1.0)
#' pmcmc(current_state, scorer)
#' 
#' @param proposal Proposal function. Default is the DefaultProposal.
#' @param temperature Numeric value representing the temperature to raise the 
#' score to.
#' @param prerejection Boolean flag to reject due to the proposal disobeying the 
#' black or white lists. Only set to FALSE if you want to understand
#' how often you are proposing states that disobey the black or white lists. Can 
#' be useful for debugging or understanding the efficiency of specific proposal 
#' distributions.
#' @param verbose Flag to pass MCMC information.
#'
#' @returns Function that takes the current state and scorer that outputs a new
#' state.
#' 
#' @export
PartitionMCMC <- function(proposal = DefaultProposal(), 
                          temperature = 1.0, 
                          prerejection = TRUE,
                          verbose = TRUE) {
  
  beta <- 1.0/temperature
  
  function(current_state, scorer) {
    
    if (prerejection) {
      
      obeys <- FALSE
      while (!obeys) {
        proposed <- proposal(current_state$state)
        
        black_obeyed <- CheckBlacklistObeyed(proposed$state, scorer$blacklist)
        white_obeyed <- CheckWhitelistObeyed(proposed$state, scorer$whitelist)
        
        if (black_obeyed & white_obeyed)
          obeys <- TRUE
      }
      
      current_state$proposal_info <- proposed$proposal_info
      
    } else {
      
      proposed <- proposal(current_state$state)
      current_state$proposal_info <- proposed$proposal_info
      
      # This rejects the disobeying proposals but records information about that
      # proposal.
      black_obeyed <- CheckBlacklistObeyed(proposed$state, scorer$blacklist)
      white_obeyed <- CheckWhitelistObeyed(proposed$state, scorer$whitelist)
      if ((!black_obeyed | !white_obeyed) & verbose) {
        current_state$mcmc_info <- list(
          accept = FALSE,
          white_obeyed = white_obeyed,
          black_obeyed = black_obeyed,
          jac = NULL,
          mhr = NULL
        )
        
        return(current_state)
      }
      
    }
    
    # Use Metropolis-Hastings to accept the obeying states.
    log_score_diff <- ScoreDiff(current_state$state, proposed$state, 
                                scorer, proposed$rescore_nodes)
    
    jac <- log(proposed$current_nbd) - log(proposed$new_nbd)
    mhr <- beta*log_score_diff
    log_r <- jac + mhr
      
    accept <- AcceptProposal(log_r)
    if (accept) {
      current_state$state <- proposed$state
      current_state$log_score <- current_state$log_score + log_score_diff
    }
    
    if (verbose)
      current_state$mcmc_info <- list(accept = accept,
                                      white_obeyed = TRUE,
                                      black_obeyed = TRUE,
                                      jac = jac,
                                      mhr = mhr)
    
    
    return(current_state)
  }
}

#' Metropolis-Hastings acceptance.
#' 
#' @param log_r Log of Metropolis-Hastings ratio.
#' @return accept A boolean indicating whether we accept the proposal.
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
