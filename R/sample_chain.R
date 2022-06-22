#' Sample chain.
#' 
#' @examples 
#' results <- SampleChain(100, partitioned_nodes, transition = PartitionMCMC(scorer = scorer_1))
#' 
#' @param n_results Number of saved states.
#' @param init_state An initial state that can be passed to transition.
#' @param transition A transition function.
#' @param scorer A scorer object.
#' @param n_thin Number of steps between saved states.
#' 
#' @returns List of results.
#' 
#' @export
SampleChain <- function(n_results, init_state, transition, scorer, n_thin = 1) {
  
  init_log_score <- ScoreLabelledPartition(init_state, scorer)
  trace <- list(state = list(init_state), log_score = init_log_score, 
                proposal_info = list(), mcmc_info = list())
  
  current_state <- list(state = init_state, log_score = init_log_score, 
                        proposal_info = NULL, mcmc_info = NULL)
  for (i in 2:n_results) {
    for (j in 1:n_thin) {
      current_state <- transition(current_state, scorer)
      
      trace$proposal_info <- append(trace$proposal_info, current_state$proposal_info)
      trace$mcmc_info <- append(trace$mcmc_info, current_state$mcmc_info)
    }
    trace$state[[i]] <- current_state$state
    trace$log_score <- c(trace$log_score, current_state$log_score)
  }
  
  return(trace)
}
