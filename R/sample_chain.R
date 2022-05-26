#' Sample chain.
#' 
#' @examples 
#' trace <- SampleChain(100, partitioned_nodes, transition = PartitionMCMC, 
#' proposal = ProposePartitionSplitJoin, scorer = scorer_1, 
#' n_thin = 1)
#' 
#' @param n_results Number of saved states.
#' @param init_state An initial state that can be passed to transition.
#' @param transition A transition function.
#' @param scorer A scorer object.
#' @param n_thin Number of steps between saved states.
#' 
#' @returns A list of states and log_score.
#' 
#' @export
SampleChain <- function(n_results, init_state, transition, proposal, scorer, 
                        n_thin = 1) {
  
  init_log_score <- ScoreLabelledPartition(init_state, scorer)
  trace <- list(
    state = c(init_state), 
    log_score = c(init_log_score)
    )
  
  current_state <- list(state = init_state, log_score = init_log_score)
  for (i in 2:n_results) {
    for (j in 1:n_thin) {
      current_state <- transition(current_state, proposal, scorer)
    }
    trace$state[[i]] <- current_state$state
    trace$log_score[[i]] <- current_state$log_score
  }
  
  return(trace)
}
