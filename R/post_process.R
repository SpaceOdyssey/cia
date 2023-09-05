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
  
  if (methods::is(chains, 'dagmc_chains')) {
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
                           same_plot = TRUE, col = NULL, ...) UseMethod('PlotScoreTrace')

#' @export
PlotScoreTrace.dagmc_chains <- function(chains, attribute = 'log_score', n_burnin = 0, 
                                        same_plot = TRUE, col = NULL, ...) {
  
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
}

#' @export
PlotScoreTrace.dagmc_chain <- function(chains, attribute = 'log_score', n_burnin = 0, 
                                        same_plot = TRUE, col = NULL, ...) {
  
  if (is.null(col))
    col <- 'black'
  
  n_within <- length(chains[[attribute]])
  plot(chains[[attribute]][(1 + n_burnin):n_within], col = col, ...)
}


#' Sample DAGs from labelled partitions.
#' 
#' @param partitions A dagms_chains, dagmc_chain, or matrix.
#' @param scorer A scorer object.
#' 
#' @export
PartitiontoDAG <- function(partitions, scorer) UseMethod('PartitiontoDAG')

#' @export
PartitiontoDAG.dagmc_chains <- function(partitions, scorer) {
  
  n_chains <- length(partitions)
  
  cl <- parallel::makeCluster(n_chains)
  doParallel::registerDoParallel(cl)
  i <- NULL
  chains <- foreach::foreach(i = 1:n_chains) %dopar% {
    PartitiontoDAG(partitions[[i]], scorer)
  }
  parallel::stopCluster(cl)
  
  chains <- new_dagmc_chains(chains)
  
  return(chains)
}

#' @export
PartitiontoDAG.dagmc_chain <- function(partitions, scorer) {
  
  n_results <- length(partitions$state)
  
  chain <- list()
  chain$state <- list()
  chain$log_score <- vector('numeric', length = n_results)
  
  for (i in 1:n_results) {
    dag <- SampleDAGFromLabelledPartition(partitions$state[[i]], scorer)
    chain$state[[i]] <- dag$state
    chain$log_score[i] <- dag$log_score
  }
  
  chain <- new_dagmc_chain(chain)
  
  return(chain)
}

#' @export
PartitiontoDAG.matrix <- function(partitions, scorer) {
  dag <- SampleDAGFromLabelledPartition(partitions, scorer)
  
  return(dag)
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
CalculateAcceptanceRates <- function(chains, group_by = NULL) UseMethod('CalculateAcceptanceRates')

#' @export
CalculateAcceptanceRates.dagmc_chains <- function(chains, group_by = NULL) { 
  n_chains <- length(chains)
  chain_info <- list()
  for (i in 1:n_chains) {
    chain_info[[i]] <- dplyr::bind_cols(
      proposal_used = sapply(chains[[i]]$proposal_info, function(x) x$proposal_used),
      accept = sapply(chains[[i]]$mcmc_info, function(x) x$accept)
    )
  }
  chain_info <- dplyr::bind_rows(chain_info)
  
  accept_summary <- chain_info |>
    dplyr::group_by_at(group_by) |>
    dplyr::summarise(mean_accept = mean(.data$accept), 
                     n_accept = sum(.data$accept), 
                     n_total = dplyr::n())
  
  return(accept_summary)
}

#' @export
CalculateAcceptanceRates.dagmc_chain <- function(chains, group_by = NULL) { 
  chains <- new_dagmc_chains(chains)
  accept_summary <- CalculateAcceptanceRates(chains)
  
  return(accept_summary)
}

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

#' Calculate marginalised edge probabilities.
#' 
#' Calculate the probability of a given edge (\eqn{E}) given the data which
#' is given by, \deqn{p(E|D) = \sum_\mathcal{G} p(E|G)p(G|D)}).
#' 
#' @param collection A collection object where states are DAGs. See CollectUniqueObjects.
#' 
#' @returns p_edge An adjacency matrix representing the edge probabilities.
#' 
#' @export
CalculateEdgeProbabilities <- function(collection) {
  
  dags <- simplify2array(collection$state)
  p_dag <- exp(collection$log_norm_state_score)
  for (i in 1:length(p_dag))
    dags[, , i] <- dags[, , i]*p_dag[i]
  p_edge <- apply(dags, c(1, 2), sum)
  
  return(p_edge)
}

#' Collect DAG feature probability.
#' 
#' @description
#' Calculate the feature (\eqn{f}) probability whereby 
#' \eqn{p(f|D) = \sum_\mathcal{G \in G} p(G|D)p(f|G)}.
#' 
#' @param collection A collection of unique objects. See CollectUniqueObjects.
#' @param p_feature A function that takes an adjacency matrix and collection object 
#' and returns a numeric value equal to p(f|G). Therefore, it must be of the 
#' form p_feature(dag).
#' 
#' @returns p_post_feature A numeric value representing the posterior probability
#' of the feature.
#' 
#' @export
CalculateFeatureProbability <- function(collection, p_feature) {
  
  p_post_feature <- 0.0
  p_dag <- exp(collection$log_norm_dag_score)
  for (i in 1:length(p_dag))
    p_post_feature <- p_post_feature + p_dag[i]*p_feature(collection$dag[[i]])
  
  return(p_post_feature)
}
