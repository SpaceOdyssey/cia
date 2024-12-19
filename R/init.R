#' Initialise states for SampleChains.

#' Initialise partition state for SampleChains.
#' 
#' @param nodes A character vector of node names.
#' @param scorer A scorer object.
#' @param init_state A data.frame representing a partition. Default is NULL.
#' @param n_parallel_chains Number of parallel chains to be run in SampleChains.
#' Default is 2.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer,
#'   data = data
#'   )
#' InitPartition(colnames(data), scorer)
#' 
#' @export
InitPartition <- function(nodes, scorer, 
                          init_state = NULL, n_parallel_chains = 2) {
  
  InitSinglePart <- function(nodes, scorer, init_state = NULL) {

    if (is.null(init_state)) {
      dag <- UniformlySampleDAG(nodes)
      if (!is.null(scorer$blacklist)) {
        dag <- dag * !scorer$blacklist
      }
      if (!is.null(scorer$whitelist)) {
        dag[scorer$whitelist] <- 1
      }    
      init_state <- DAGtoPartition(dag)
    } else if (is.matrix(init_state)) {  # Assume adjacency matrix.
      init_state <- DAGtoPartition(init_state)
      stopifnot(CheckWhitelistObeyed(init_state))
      stopifnot(CheckBlacklistObeyed(init_state))
    } else {
      stopifnot(is.data.frame(init_state)) # State must be partition.
      stopifnot(CheckWhitelistObeyed(init_state))
      stopifnot(CheckBlacklistObeyed(init_state))      
    }
    
    current_state <- list(state = init_state,
                          log_score = ScoreLabelledPartition(init_state, scorer),
                          proposal_info = NULL,
                          mcmc_info = NULL)
    
    return(current_state)
  }
  
  if (n_parallel_chains == 1) {
    return(InitSinglePart(nodes, scorer, init_state))
  } else {
    current_state <- list()
    for (i in 1:n_parallel_chains)
      current_state[[i]] <- InitSinglePart(nodes, scorer, init_state)
  }
  
  return(current_state)
}

#' Initialise partition state for SampleChains. 
#' `r lifecycle::badge("experimental")`
#'
#' @param nodes A character vector of node names.
#' @param scorer A scorer object.
#' @param init_state Coupled partition state. Default is NULL.
#' @param n_coupled_chains Number of coupled chains. Default is 4.
#' @param n_parallel_chains Number of parallel chains to be run in SampleChains.
#' Default is 2.
#'
#' @examples
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer,
#'   data = bnlearn::learning.test
#'   )
#' nodes <- names(bnlearn::learning.test)
#' 
#' InitCoupledPartition(nodes, scorer)
#'
#' @export
InitCoupledPartition <- function(nodes, scorer, init_state = NULL,
                                 n_coupled_chains = 4, n_parallel_chains = 2) {
  
  
  InitSingleCoupledPart <- function(nodes, scorer, init_state = init_state,
                                    n_coupled_chains = n_coupled_chains) {
    
    mcmc_info <- list()
    for (i in 1:n_coupled_chains) {
      
      if (is.null(init_state)) {
        dag <- UniformlySampleDAG(nodes)
        if (!is.null(scorer$blacklist)) {
          dag <- dag * !scorer$blacklist
        }
        if (!is.null(scorer$whitelist)) {
          dag[scorer$whitelist] <- 1
        }    
        init_state <- DAGtoPartition(dag)
      } else if (is.matrix(init_state)) {  # Assume adjacency matrix.
        init_state <- DAGtoPartition(init_state)
        stopifnot(CheckWhitelistObeyed(init_state))
        stopifnot(CheckBlacklistObeyed(init_state))
      } else {
        stopifnot(is.data.frame(init_state)) # State must be partition.
        stopifnot(CheckWhitelistObeyed(init_state))
        stopifnot(CheckBlacklistObeyed(init_state))      
      }
      
      part <- DAGtoPartition(dag)
      log_score <- ScoreLabelledPartition(part, scorer)
      mcmc_info[[i]] <- list(state = part, log_score = log_score)
    }
    
    current_state <- list(
      state = mcmc_info[[1]]$state,
      log_score = mcmc_info[[1]]$log_score,
      proposal_info = NULL,
      mcmc_info = mcmc_info
    )
    
    return(current_state)
  }
  
  if (n_parallel_chains == 1) {
    return(InitSingleCoupledPart(nodes, scorer, init_state, n_coupled_chains))
  } else {
    current_state <- list()
    for (i in 1:n_parallel_chains)
      current_state[[i]] <- InitSingleCoupledPart(nodes, scorer, init_state, n_coupled_chains)
  }
  
  return(current_state)
}
