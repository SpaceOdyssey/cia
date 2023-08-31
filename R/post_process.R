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

#' Flatten list of chains.
#' 
#' @param chains MCMC chains.
#' 
#' @export
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

#' Collect unique objects.
#' 
#' @description
#' Get the unique set of states and DAGs along with their log score.
#' 
#' @details This gets the unique set of states and DAGs which are 
#' referred to as objects (\eqn{\mathcal{O}}). Then estimates the log of the
#' normalisation constant assuming 
#' \eqn{\tilde{Z}_\mathcal{O} = \Sigma_s^S p(\mathcal{O}_s)p(D | \mathcal{O}_s)} where 
#' \eqn{\{\mathcal{O}_1, \mathcal{O}_2, \mathcal{O}_3, ..., \mathcal{O}_S\}} is 
#' the set of unique objects in the chain. This assumes that you have captured the 
#' most probable objects, such that \eqn{\tilde{Z}_\mathcal{O}} is approximately equal to 
#' the true evidence \eqn{Z = \Sigma_{G \in \mathcal{G}} p(G)p(D | G)} where you 
#' sum across all possible DAGs (\eqn{\mathcal{G}}). This also makes the 
#' assumption that the exponential of the score is proportional to the posterior
#' probability, such that 
#' \deqn{p(G|D) \propto p(G)p(D | G) = \prod_i \exp(\text{score}(X_i, \text{Pa}_G(X_i) | D))}
#' where \eqn{\text{Pa}_G(X_i)} is the parents set for node \eqn{X_i}.
#' 
#' We calculate the estimator using both the states (e.g., labelled partitions)
#' and DAGs. The estimator using the labelled partitions is more accurate as it
#' includes the sum over a greater number of DAGs. However, they should be 
#' approximately the same value. If they are not, then you probably haven't 
#' sampled enough DAGs from your states.
#' 
#' After the normalisation constant has been estimated we then estimate the 
#' log probability of each object as,
#' \deqn{\log(p(\mathcal{O}|D)) = \log(p(\mathcal{O})p(D|\mathcal{O})) - \log(\tilde{Z_\mathcal{O}})}.
#' 
#' @param chain A chain that includes a DAG per sample.
#' 
#' @returns dag_collection: A list with entries:
#' \itemize{
#'  \item states: List of unique states.
#'  \item log_evidence_states: Numeric value representing the evidence calculated from 
#'  the states.
#'  \item log_state_scores: Vector with the log scores for each state.
#'  \item dags: List of unique DAGs.
#'  \item dag_scores: Vector with the log scores for each DAG.
#'  \item log_norm_dag_scores: Vector of normalised dag scores.
#'  \item log_evidence_dags: Numeric value representing the evidence calculated from 
#'  the DAGs.
#' }
#' 
#' @export
CollectUniqueObjects <- function(chain) {
  
  # States calculations.
  states <- chain$state
  state_scores <- chain$log_score
  state_hashes <- states |>
    lapply(rlang::hash) |>
    unlist()
  
  # Summarise unique states.
  state_ihash <- match(unique(state_hashes), state_hashes)
  unique_states <- states[state_ihash]
  unique_state_scores <- state_scores[state_ihash]
  log_evidence_states <- LogSumExp(unique_state_scores)
  log_norm_state_scores <- unique_state_scores - log_evidence_states
  
  # DAG calculations.
  dags <- chain$dag
  dag_scores <- unlist(chain$log_dag_score)
  dag_hashes <- dags |>
    lapply(rlang::hash) |>
    unlist()
  
  # Summarise unique DAGs.
  dag_ihash <- match(unique(dag_hashes), dag_hashes)
  unique_dag_scores <- dag_scores[dag_ihash]
  unique_dags <- dags[dag_ihash]
  log_evidence_dags <- LogSumExp(unique_dag_scores)
  
  log_norm_dag_scores <- unique_dag_scores - log_evidence_dags
  
  col <- list(states = unique_states,
              log_evidence_states = log_evidence_states,
              log_state_scores = unique_state_scores,
              log_norm_state_scores = log_norm_state_scores,
              dags = unique_dags,
              dag_scores = unique_dag_scores,
              log_norm_dag_scores = log_norm_dag_scores,
              log_evidence_dags = log_evidence_dags)
  
  return(col)
}
