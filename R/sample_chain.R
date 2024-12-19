#' Sample chains
#' 
#' @param n_results Number of saved states per chain.
#' @param init_state An initial state that can be passed to transition. This can
#' be a single state or a list of states for each parallel chain.
#' @param transition A transition function.
#' @param scorer A scorer object.
#' @param n_thin Number of steps between saved states.
#' @param n_parallel_chains Number of chains to run in parallel. Default is 2.
#' 
#' @returns A cia_chains object.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer,
#'   data = data
#'   )
#' init_state <- InitPartition(colnames(data), scorer)
#' 
#' results <- SampleChains(10, init_state, PartitionMCMC(), scorer)
#' 
#' @export
SampleChains <- function(n_results, init_state, transition, scorer, n_thin = 1, 
                         n_parallel_chains = 2) {
  
  if (!is.list(init_state))
    print(
      paste0(
        "init_state must be a list of states.",
        "We recommend using the available init functions (e.g., InitPartition)."
        )
      )
  
  cl <- parallel::makeCluster(n_parallel_chains, outfile = "")
  doParallel::registerDoParallel(cl)
  
  i <- NULL
  chains <- foreach::foreach(i = 1:n_parallel_chains) %dopar% {
    
    if ("state" %in% names(init_state))
      init_state_each = init_state
    else 
      init_state_each = init_state[[i]]

    SampleChain(n_results, init_state_each, transition, scorer, n_thin, 
                name = paste('Chain', i))
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
#' @param init_state An initial state. Recommended to use the available 
#' initialisation functions (e.g., InitPartition, InitCoupledPartition).
#' @param transition A transition function.
#' @param scorer A scorer object.
#' @param n_thin Number of steps between saved states.
#' 
#' @returns chain A cia_chain object.
#' 
#' @noRd
SampleChain <- function(n_results, init_state, transition, scorer, n_thin = 1,
                        name = NULL) {
  
  # Setup objects to store results.
  states <- vector('list', n_results)
  log_scores <- numeric(n_results)
  proposal_info <- vector('list', n_results*n_thin)
  mcmc_info <- vector('list', n_results*n_thin)
  
  # Setup initial state.
  if (!CheckWhitelistObeyed(init_state$state, scorer$whitelist))
    stop('Initial state does not obey whitelist.')
  
  if (!CheckBlacklistObeyed(init_state$state, scorer$blacklist))
    stop('Initial state does not obey blacklist.')
  
  if (is.data.frame(init_state)) {
    stop("Supplying a data.frame is no longer supported. ",
         "Please use the Init functions (e.g. InitPartition)")
  } else {
    current_state <- init_state
  }
  
  sprintf('%s started sampling.', name)
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
    
    # Printing with do par doesn't work.
    if (i %% (n_results / 10) == 0) {
      if (!is.null(name)) {
        sprintf('%s Progress: %i%%', name, round(100*i/n_results, 0))
      } else {
        sprintf('Progress: %i%%', round(100*i/n_results, 0))
      }
    }
  }
  
  chain <- new_cia_chain(
    list(state = states, 
         log_score = log_scores, 
         proposal_info = proposal_info, 
         mcmc_info = mcmc_info)
  )
  
  return(chain)
}
