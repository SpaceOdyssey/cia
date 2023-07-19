#' Analysis of chains.

#' Equilibrium states.
#' 
#' This allows you to remove a burnin and thin the chains after processing.
#' 
#' @param chains MCMC chains.
#' @param n_burnin Number of steps to remove at the start as a burnin. Default is 0.
#' @param n_thin Number of steps between retained states. Default is 1.
#' 
#' @export
PostProcessChains <- function(chains, n_burnin = 0, n_thin = 1) {
  
  if (MultipleChains(chains)) {
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

#' Plot the score trace.
#' 
#' @param chains MCMC chains.
#' @param attribute Name of attribute to plot. Default is "log_score".
#' @param n_burnin Number of steps to remove as burnin.
#' @param same_plot Whether to plot on the same figure or on multiple figures.
#' @param col A string representing a color for a single chain or a vector of 
#' strings to cycle through for multiple chains.
#' @param ... Extra parameters to pass to the plot and graphics::line functions.
#' 
#' @export
PlotScoreTrace <- function(chains, attribute = 'log_score', n_burnin = 0, 
                           same_plot = TRUE, col = NULL, ...) {
  
  if (MultipleChains(chains)) {
    if (is.null(col))
      col <- c('black', 'green', 'red', 'blue', 'brown', 'darkviolet', 
                  'darkgoldenrod', 'deeppink')
    
    n_within <- length(chains[[1]][[attribute]])
    plot(chains[[1]][[attribute]][(1 + n_burnin):n_within], col = col[1], ...)
    
    n_chains <- length(chains)
    if (n_chains > 1) {
      for (i in 2:length(chains)) {
        n_within <- length(chains[[i]][[attribute]])
        if (same_plot) {
          graphics::lines(chains[[i]][[attribute]][(1 + n_burnin):n_within], 
                          col = col[i %% length(col)], ...)
        } else {
          plot(chains[[i]][[attribute]][(1 + n_burnin):n_within], 
               col = col[i %% length(col)], ...)
        }
      }
    }
  } else {
    if (is.null(col))
      col <- 'black'
    
    n_within <- length(chains[[attribute]])
    plot(chains[[attribute]][(1 + n_burnin):n_within], col = col, ...)
  }
}

#' Sampled DAG from chains.
#' 
#' @param chains MCMC chains.
#' @param scorer Scorer object.
#'
#' @return Chains with sample dags and their corresponding score.
#'
#' @export
SampleChainDAGs <- function(chains, scorer) {
  
  if (MultipleChains(chains)) {
    for (i in 1:length(chains)) {
      chains[[i]] <- SampleSingleChainDAGs(chains[[i]], scorer)
    }
  } else {
    chains <- SampleSingleChainDAGs(chains, scorer)
  }
  
  return(chains)
}

#' Sample DAGs from single chain.
#' 
#' @noRd
SampleSingleChainDAGs <- function(chain, scorer) {
  chain$dag <- lapply(chain$state, SampleDAGFromLabelledPartition, scorer = scorer)
  chain$log_dag_score <- unlist(lapply(chain$dag, ScoreDAG, scorer = scorer))
  
  return(chain)
}

#' Check whether multiple chains or not.
#' 
#' @noRd
MultipleChains <- function(chains) {
  
  if (is.null(names(chains)))
    return(TRUE)
  else
    return(FALSE)
}

#' Calculate acceptance rates per proposal. 
#' 
#' This makes the assumption that the proposal has saved a variable "proposal_used"
#' and mcmc has saved a variable 'accept'.
#' 
#' @param chains MCMC chains.
#' @param group_by Vector of strings that are in c("chain", "proposal_used"). 
#' Default is NULL which will return the acceptance rates marginalised over
#' chains and the proposal used.
#' 
#' @returns Summary of acceptance rates per grouping.
#'
#' @export
CalculateAcceptanceRates <- function(chains, group_by = NULL) {
  
  if (MultipleChains(chains)) {
    n_chains <- length(chains)
  } else {
    n_chains <- 1
    chains <- list(chains)
  }
  
  chain_info <- list()
  for (i in 1:n_chains) {
    chain_info[[i]] <- dplyr::bind_cols(
      proposal_used = sapply(chains[[i]]$proposal_info, function(x) x$proposal_used),
      accept = sapply(chains[[i]]$mcmc_info, function(x) x$accept)
    )
  }
  chain_info <- dplyr::bind_rows(chain_info)
  
  accept_summary <- chain_info %>%
    dplyr::group_by_at(group_by) %>%
    dplyr::summarise(mean_accept = mean(.data$accept), 
                     n_accept = sum(.data$accept), 
                     n_total = dplyr::n())
  
  return(accept_summary)
}

#' Flatten list of chains. Not exporting at the moment. Not sure if this is how 
#' I want this implemented. 
#' 
#' @param chains MCMC chains.
#' 
#' @noRd
FlattenChains <- function(chains) {
  
  stopifnot(MultipleChains(chains))
  
  chain <- list()
  for (name in names(chains[[i]])) {
    chain[[name]] <- list()
    for (i in 1:length(chains)) {
      chain[[name]] <- c(chain[[name]], chains[[i]][[name]])
    }
  }
  
  return(chain)
}
