#' Sample chains
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
#' 
#' @param n_results Number of saved states per chain.
#' @param init_state An initial state that can be passed to transition. This can
#' be a single state or a list of states for each parallel chain.
#' @param transition A transition function.
#' @param scorer A scorer object.
#' @param n_thin Number of steps between saved states.
#' @param n_parallel_chains Number of chains to run in parallel. Default is 2.
#' 
#' @returns chains A cia_chains object.
#' 
#' @export
SampleChains <- function(n_results, init_state, transition, scorer, n_thin = 1, 
                         n_parallel_chains = 2) {
  
  cl <- parallel::makeCluster(n_parallel_chains)
  doParallel::registerDoParallel(cl)
  
  i <- NULL
  chains <- foreach::foreach(i = 1:n_parallel_chains) %dopar% {
    
    if (is.data.frame(init_state)) {
      init_state_each <- init_state
    } else {
      init_state_each <- init_state[[i]]
    }

    SampleChain(n_results, init_state_each, transition, scorer, n_thin)
  }
  parallel::stopCluster(cl)
  
  chains <- new_cia_chains(chains)
  
  return(chains)
}

#' Sample a single chain.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- DAGtoPartition(dag)
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
#' @returns chain A cia_chain object.
#' 
#' @noRd
SampleChain <- function(n_results, init_state, transition, scorer, n_thin = 1) {
  
  # Setup objects to store results.
  states <- vector('list', n_results)
  log_scores <- numeric(n_results)
  proposal_info <- vector('list', n_results*n_thin)
  mcmc_info <- vector('list', n_results*n_thin)
  
  # Setup initial state.
  if (!CheckWhitelistObeyed(init_state, scorer$whitelist))
    stop('Initial state does not obey whitelist.')
  
  if (!CheckBlacklistObeyed(init_state, scorer$blacklist))
    stop('Initial state does not obey blacklist.')
  
  init_log_score <- ScoreLabelledPartition(init_state, scorer)
  current_state <- list(state = init_state,
                        log_score = init_log_score,
                        proposal_info = NULL,
                        mcmc_info = NULL)
  
  # Run MCMC to return n_results.
  for (i in 1:n_results) {
    for (j in 1:n_thin) {
      current_state <- transition(current_state, scorer)
      
      i_info <- 1 + (i - 1)*n_thin + (j - 1)
      proposal_info[[i_info]] <- current_state$proposal_info
      mcmc_info[[i_info]] <- current_state$mcmc_info
    }
    states[[i]] <- current_state$state
    log_scores[[i]] <- current_state$log_score
  }
  
  chain <- new_cia_chain(
    list(state = states, 
         log_score = log_scores, 
         proposal_info = proposal_info, 
         mcmc_info = mcmc_info)
  )
  
  return(chain)
}
