#' Sample multiple chains in parallel.
#' 
#' @examples
#' data = bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' 
#' scorer <- list(
#'   scorer = BNLearnScorer, 
#'   parameters = list(data = data)
#'   )
#' 
#' results <- SampleChains(10, partitioned_nodes, PartitionMCMC(), scorer)
#' 
#' @param n_results Number of saved states.
#' @param init_state An initial state that can be passed to transition. This can
#' be a single state or a list of states for each parallel chain.
#' @param transition A transition function.
#' @param scorer A scorer object.
#' @param n_thin Number of steps between saved states.
#' @param n_parallel_chains Number of chains to run in parallel. Default is 2.
#' 
#' @returns List of results.
#' 
#' @export
SampleChains <- function(n_results, init_state, transition, scorer, n_thin = 1, 
                         n_parallel_chains = 2) {
  
  doParallel::registerDoParallel(n_parallel_chains)
  
  trace <- foreach::foreach(i = 1:n_parallel_chains) %dopar% {
    
    if (is.data.frame(init_state)) {
      init_state_each <- init_state
    } else {
      init_state_each <- init_state[[i]]
    }

    SampleChain(n_results, init_state_each, transition, scorer)
  }
  
  return(trace)
}

#' Sample a single chain.
#' 
#' @examples 
#' data = bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' 
#' scorer_1 <- list(
#'   scorer = BNLearnScorer, 
#'   parameters = list(data = data)
#'   )
#' 
#' results <- SampleChain(10, partitioned_nodes, PartitionMCMC(), scorer_1)
#' 
#' @param n_results Number of saved states.
#' @param init_state An initial state that can be passed to transition.
#' @param transition A transition function.
#' @param scorer A scorer object.
#' @param n_thin Number of steps between saved states.
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

