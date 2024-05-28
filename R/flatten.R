#' Flatten list of chains.
#' 
#' @param chains MCMC chains.
#' 
#' @export
FlattenChains <- function(chains) {
  
  stopifnot(methods::is(chains, 'dagmc_chains'))
  
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
  
  chain <- new_dagmc_chain(chain)
  
  return(chain)
}
