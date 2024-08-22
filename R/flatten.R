#' Flatten list of chains.
#' 
#' @param chains MCMC chains.
#' 
#' @export
FlattenChains <- function(chains) UseMethod('FlattenChains')

#' @export
FlattenChains.cia_chains <- function(chains) {
  
  n_chains <- length(chains)
  chain <- list()
  
  chain$state <- list()
  chain$log_score <- c()
  chain$proposal_info <- list()
  chain$mcmc_info <- list()
  for (i in 1:n_chains) {
    chain$state <- c(chain$state, chains[[i]]$state)
    chain$log_score <- c(chain$log_score, chains[[i]]$log_score)
    chain$proposal_info <- c(chain$proposal_info, chains[[i]]$proposal_info)
    chain$mcmc_info <- c(chain$mcmc_info, chains[[i]]$mcmc_info)
  }
  
  chain <- new_cia_chain(chain)
  
  return(chain)
}

#' @export
FlattenChains.cia_post_chains <- function(chains) {
  
  chain <- chains |>
    do.call(rbind, args = _) |>
    new_cia_post_chain()
  
  return(chain)
}

