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

#' Collect DAGs.
#' 
#' Get the unique set of DAGs along with their log_score. It also estimates
#' the normalised log score assuming \eqn{\tilde{Z} = \Sigma_s^S p(G_s)p(D | G_s)} 
#' where \eqn{\{G_1, G_2, G_3, ..., G_S\}} is the set of unique DAGs in the 
#' the chain. This assumes that you have captured the most probable graphs, such 
#' that \eqn{\tilde{Z}} is approximately equal to the true evidence 
#' \eqn{Z = \Sigma_{G \in \mathcal{G}} p(G)p(D | G)} 
#' where you sum across all possible graphs \eqn{\mathcal{G}}. This
#' also makes the assumption that the scoring method used is proportional to
#' the posterior probability; \eqn{\text{score}(G, D) \propto p(G)p(G | D)}.
#' 
#' @param post_chain A chain that includes a DAG per sample.
#' 
#' @returns dag_collection A list with entries:
#'  dag: A list of all unique DAGs within the sample.
#'  log_score: A vector with the log_scores for each DAG.
#'  log_evidence: A numeric value representing the evidence \eqn{log(\tilde{Z}) = \Sigma_s^S log(p(G_s)p(D | G_s))}.
#'  log_norm_score: A vector of normalised log_scores for each DAG \eqn{G_s} using \eqn{p(G_s | D) = log(p(G_s)p(D | G_s)) - log(\tilde{Z})}.
#' 
#' @export
CollectDags <- function(post_chain) {
  
  dags <- post_chain$dag
  log_score <- unlist(post_chain$log_score)
  
  # Assign hashs for each dag to simplify the next calculations.
  hashs <- dags |>
    lapply(rlang::hash) |>
    unlist()
  
  # Summarise unique DAGs.
  ihash <- match(unique(hashs), hashs)
  unique_log_scores <- log_score[ihash]
  unique_dags <- dags[ihash]
  
  log_evidence <- LogSumExp(unique_log_scores)
  log_norm_score <- unique_log_scores - log_evidence
  
  col <- list(dag = unique_dags,
              log_score = unique_log_scores,
              log_norm_score = log_norm_score,
              log_evidence = log_evidence)
  
  return(col)
}
