#' Index chains for further analysis.
#' 
#' This allows you to remove a burnin and thin the chains after processing. This
#' is mostly redundant as you can now index the cia_chain(s) objects directly.
#' 
#' @param chains MCMC chains.
#' @param n_burnin Number of steps to remove at the start as a burnin. Default is 0.
#' @param n_thin Number of steps between retained states. Default is 1.
#' 
#' @export
PostProcessChains <- function(chains, n_burnin = 0, n_thin = 1) {
  
  if (methods::is(chains, 'cia_chains')) {
    for (i in 1:length(chains)) {
      chains[[i]] <- PostProcessChain(chains[[i]], n_burnin, n_thin)
    }
  } else {
    chains <- PostProcessChain(chains, n_burnin, n_thin)
  }
  
  return(chains)
}

#' Post process for a single chain.
#' 
#' @noRd
PostProcessChain <- function(chain, n_burnin, n_thin) {
  
  n_within <- length(chain$log_score)
  for (name in names(chain))
    chain[[name]] <- chain[[name]][seq(1 + n_burnin, n_within, n_thin)]
  
  return(chain)
}
