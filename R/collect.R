#' Collect unique objects.
#' 
#' @description
#' Get the unique set of states and DAGs along with their log score.
#' 
#' @details This gets the unique set of states in a dagmc_object
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
#' After the normalisation constant has been estimated we then estimate the 
#' log probability of each object as,
#' \deqn{\log(p(\mathcal{O}|D)) = \log(p(\mathcal{O})p(D|\mathcal{O})) - \log(\tilde{Z_\mathcal{O}})}.
#' 
#' @param x A dagmc_chains or dagmc_chain object.
#' 
#' @returns dag_collection: A list with entries:
#' \itemize{
#'  \item state: List of unique states.
#'  \item log_evidence_state: Numeric value representing the evidence calculated from 
#'  the states.
#'  \item log_state_score: Vector with the log scores for each state.
#' }
#' 
#' @export
CollectUniqueObjects <- function(x) UseMethod('CollectUniqueObjects')

#' @export
CollectUniqueObjects.dagmc_chains <- function(x) {
  
  collections <- list()
  n_chains <- length(x)
  for (i in 1:n_chains) {
    collections[[i]] <- CollectUniqueObjects(x[[i]])
    collections[[i]] <- new_dagmc_collection(collections[[i]])
  }
  
  collections <- new_dagmc_collections(collections)
  
  return(collections)
}

#' @export
CollectUniqueObjects.dagmc_chain <- function(x) {
  
  # States calculations.
  states <- x$state
  state_scores <- x$log_score
  state_hashes <- states |>
    lapply(rlang::hash) |>
    unlist()
  
  # Summarise unique states.
  state_ihash <- match(unique(state_hashes), state_hashes)
  unique_states <- states[state_ihash]
  unique_state_scores <- state_scores[state_ihash]
  log_evidence_states <- LogSumExp(unique_state_scores)
  log_norm_state_scores <- unique_state_scores - log_evidence_states
  
  collection <- list(state = unique_states,
                     log_evidence_state = log_evidence_states,
                     log_state_score = unique_state_scores,
                     log_norm_state_score = log_norm_state_scores)
  
  collection <- new_dagmc_collection(collection)
  
  return(collection)
}

#' Constructor for a dagmc_chains collection.
#' 
#' @param x A list corresponding to a single mcmc chain.
#' @returns dagmc_chain A dagmc_chain object.
#' 
#' @noRd
new_dagmc_collections <- function(x) {
  
  stopifnot(is.list(x))
  dagmc_collections <- structure(x, class = 'dagmc_collections')
  
  return(dagmc_collections)
}

#' Constructor for a dagmc_chain collection.
#' 
#' @param x A list corresponding to a single mcmc chain.
#' @returns dagmc_chain A dagmc_chain object.
#' 
#' @noRd
new_dagmc_collection <- function(x) {
  
  stopifnot(is.list(x))
  dagmc_collection <- structure(x, class = 'dagmc_collection')
  
  return(dagmc_collection)
}
