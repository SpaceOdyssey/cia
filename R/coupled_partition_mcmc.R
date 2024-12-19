#' Coupled Partition MCMC
#' 
#' One step implementation of the tempered partition MCMC.
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This is a constructor for a single Coupled Partition MCMC step. The function
#' constructs an environment with the proposal, inverse temperature, and verbose 
#' flag. It then returns a function that takes the current_state and a scorer 
#' object. This only allows the scores to be raised to a constant temperature
#' for every step.
#' 
#' @examples
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer,
#'   data = bnlearn::learning.test
#'   )
#' nodes <- names(bnlearn::learning.test)
#' n_coupled_chains <- 8
#' coupled_state <- InitCoupledPartition(nodes, scorer,
#'                                      n_parallel_chains = 1,
#'                                      n_coupled_chains = n_coupled_chains)
#' coupled_transition <- CoupledPartitionMCMC(
#'   proposal = DefaultProposal(p = c(0.0, 1.0, 0.0, 0.0, 0.0)),
#'   temperature = 2^(0:(n_coupled_chains - 1))
#' )
#' coupled_transition(coupled_state, scorer)
#' 
#' @param proposal Proposal function for each chain. The swap proposal is
#' dealt with internally. Default is the DefaultProposal.
#' @param temperature Numeric value representing the temperature to raise the 
#' score to. Default is c(1.0, 10.0, 100.0, 1000.0).
#' @param verbose Flag to pass MCMC information.
#'
#' @returns Function that takes the current state and scorer that outputs a new
#' state.
#' 
#' @export
CoupledPartitionMCMC <- function(proposal = DefaultProposal(),
                                 temperature = c(1.0, 10.0, 100.0, 1000.0), 
                                 verbose = TRUE) {
  
  beta <- 1.0/temperature
  
  function(current_state, scorer) {
    
    n_chains <- length(beta)
    n_states <- length(current_state$mcmc_info)
    
    for (i in 1:n_chains) {
      
      proposed <- proposal(current_state$mcmc_info[[i]]$state)
      current_state$mcmc_info[[i]]$proposal_info <- proposed$proposal_info
        
      # Reject if proposal disobeys blacklist/whitelist.
      black_obeyed <- CheckBlacklistObeyed(proposed$state, scorer$blacklist)
      white_obeyed <- CheckWhitelistObeyed(proposed$state, scorer$whitelist)
      if (!black_obeyed | !white_obeyed) {
        
        if (verbose) {
          current_state$mcmc_info[[i]]$accept = FALSE
          current_state$mcmc_info[[i]]$white_obeyed = white_obeyed
          current_state$mcmc_info[[i]]$black_obeyed = black_obeyed
          current_state$mcmc_info[[i]]$jac = FALSE
          current_state$mcmc_info[[i]]$mhr = FALSE
        }
        next
      }
      
      # Use Metropolis-Hastings to accept/reject the obeying states.
      log_score_diff <- ScoreDiff(current_state$mcmc_info[[i]]$state,
                                  proposed$state,
                                  scorer, 
                                  proposed$rescore_nodes)
      
      jac <- log(proposed$current_nbd) - log(proposed$new_nbd)
      mhr <- beta[i]*log_score_diff
      log_r <- jac + mhr
      
      accept <- AcceptProposal(log_r)
      if (accept) {
        current_state$mcmc_info[[i]]$state <- proposed$state
        current_state$mcmc_info[[i]]$log_score <- current_state$mcmc_info[[i]]$log_score + log_score_diff
      }
      
      if (verbose) {
        current_state$mcmc_info[[i]]$accept <- accept 
        current_state$mcmc_info[[i]]$white_obeyed <- TRUE 
        current_state$mcmc_info[[i]]$black_obeyed <- TRUE 
        current_state$mcmc_info[[i]]$jac <- jac
        current_state$mcmc_info[[i]]$mhr <- mhr
      }
    }
    
    # Proposal to swap states between chains.
    i_swap <- sample(1:n_chains, 1)
    if (i_swap == 1) {
      j_swap <- 2
      q_j_given_i <- 1.0
    } else if (i_swap == n_chains) {
      j_swap = n_chains - 1
      q_j_given_i <- 1.0
    } else {
      j_swap <- sample(c(i_swap - 1, i_swap + 1), 1)
      q_j_given_i <- 0.5
    }
    
    if ((j_swap == 1) | (j_swap == n_chains)) {
      q_i_given_j = 1
    } else {
      q_i_given_j = 0.5
    }
    beta_i <- beta[i_swap]
    beta_j <- beta[j_swap]
    
    x <- current_state$mcmc_info[[i_swap]]$state
    y <- current_state$mcmc_info[[j_swap]]$state
    x_log_score <- current_state$mcmc_info[[i_swap]]$log_score
    y_log_score <- current_state$mcmc_info[[j_swap]]$log_score
    
    # log_r <- beta_swap[1]*y_log_score + beta_swap[2]*x_log_score - 
    #   (beta_swap[1]*x_log_score + beta_swap[2]*y_log_score)
    jac <- log(q_i_given_j) - log(q_j_given_i)
    log_r <- (beta_i - beta_j)*(y_log_score - x_log_score) + jac
    
    accept <- AcceptProposal(log_r)
    if (accept) {
      current_state$mcmc_info[[i_swap]]$state <- y
      current_state$mcmc_info[[i_swap]]$log_score <- y_log_score
      
      current_state$mcmc_info[[j_swap]]$state <- x
      current_state$mcmc_info[[j_swap]]$log_score <- x_log_score
    }
    
    current_state$mcmc_info$mcmc_swap_info <- list(accept = accept,
                                                   state_1 = i_swap,
                                                   state_2 = j_swap)
    
    current_state$state <- current_state$mcmc_info[[1]]$state
    current_state$log_score <- current_state$mcmc_info[[1]]$log_score
    
    return(current_state)
  }
}
