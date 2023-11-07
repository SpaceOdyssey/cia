#' Calculate acceptance rates.
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
CalculateAcceptanceRates <- function(chains, group_by = NULL) UseMethod('CalculateAcceptanceRates')

#' @export
CalculateAcceptanceRates.dagmc_chains <- function(chains, group_by = NULL) { 
  n_chains <- length(chains)
  chain_info <- list()
  for (i in 1:n_chains) {
    chain_info[[i]] <- dplyr::bind_cols(
      proposal_used = sapply(chains[[i]]$proposal_info, function(x) x$proposal_used),
      accept = sapply(chains[[i]]$mcmc_info, function(x) x$accept),
      blacklist = sapply(chains[[i]]$mcmc_info, function(x) x$blacklist)
    )
  }
  chain_info <- dplyr::bind_rows(chain_info, .id = 'chain')
  
  accept_summary <- chain_info |>
    dplyr::group_by_at(group_by) |>
    dplyr::summarise(mean_accept = mean(.data$accept), 
                     n_accept = sum(.data$accept), 
                     n_total = dplyr::n(),
                     conditional_accept_true = mean(.data$accept[blacklist == TRUE]),
                     conditional_accept_false = mean(.data$accept[blacklist == FALSE]))
  
  return(accept_summary)
}

#' @export
CalculateAcceptanceRates.dagmc_chain <- function(chains, group_by = NULL) { 
  chains <- new_dagmc_chains(chains)
  accept_summary <- CalculateAcceptanceRates(chains)
  
  return(accept_summary)
}
