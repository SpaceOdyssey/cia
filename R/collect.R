#' Collect unique objects
#' 
#' @description
#' Get the unique set of states along with their log score.
#' 
#' @details This gets the unique set of states in a cia_object
#' referred to as objects (\eqn{o}). Then it estimates the probability
#' for each state using two methods. The log_sampling_prob uses the MCMC sampled 
#' frequency to estimate the posterior probability.
#' 
#' An alternative method to estimate the posterior probability for each state
#' uses the state score. This is recorded in the log_norm_state_score 
#' vector. This approach estimates the log of the normalisation constant assuming 
#' \eqn{\tilde{Z}_O = \Sigma_{s=1}^S p(o_s)p(D | o_s)} where 
#' \eqn{O = \{o_1, o_2, o_3, ..., o_S\}} is 
#' the set of unique objects in the chain. This assumes that you have captured the 
#' most probable objects, such that \eqn{\tilde{Z}_O} is approximately equal to 
#' the true evidence \eqn{Z = \Sigma_{g \in G} p(g)p(D | g)} where the 
#' sum across all possible DAGs (\eqn{G}). This also makes the 
#' assumption that the exponential of the score is proportional to the posterior
#' probability, such that 
#' \deqn{p(g|D) \propto p(g)p(D | g) = \prod_i \exp(\text{score}(X_i, \text{Pa}_g(X_i) | D))}
#' where \eqn{\text{Pa}_g(X_i)} is the parents set for node \eqn{X_i} given the 
#' graph \eqn{g}.
#' 
#' After the normalisation constant has been estimated we then estimate the 
#' log probability of each object as,
#' \deqn{\log(p(o | D)) = \log(p(o)p(D|o)) - \log(\tilde{Z}_o).}
#' 
#' Preliminary analysis suggests that the sampling frequency approach is more
#' consistent across chains when estimating marginalised edge probabilities, 
#' and therefore is our preferred method. However, more work needs to be done 
#' here.
#' 
#' @param x A cia_chains or cia_chain object.
#' 
#' @returns dag_collection: A list with entries:
#' \itemize{
#'  \item state: List of unique states.
#'  \item log_evidence_state: Numeric value representing the evidence calculated 
#'  from the states.
#'  \item log_state_score: Vector with the log scores for each state.
#'  \item log_sampling_prob: Vector with the log of the probability for each 
#'  state estimated using the MCMC sampling frequency.
#' }
#' 
#' @export
CollectUniqueObjects <- function(x) UseMethod('CollectUniqueObjects')

#' @export
CollectUniqueObjects.cia_chain <- function(x) {
  
  # States calculations.
  states <- x$state
  state_scores <- x$log_score
  state_hashes <- states |>
    lapply(rlang::hash) |>
    unlist()
  
  # Summarise unique states.
  unique_hashes <- unique(state_hashes)
  state_ihash <- match(unique_hashes, state_hashes)
  unique_states <- states[state_ihash]
  unique_state_scores <- state_scores[state_ihash]
  log_evidence_states <- LogSumExp(unique_state_scores)
  log_norm_state_scores <- unique_state_scores - log_evidence_states
  
  # Estimate probability using sampled frequency.
  log_p_unord <- log(table(state_hashes)) - log(length(state_hashes))
  log_sampling_prob <- as.vector(log_p_unord[unique_hashes])
  
  collection <- list(state = unique_states,
                     log_evidence_state = log_evidence_states,
                     log_state_score = unique_state_scores,
                     log_norm_state_score = log_norm_state_scores,
                     log_sampling_prob = log_sampling_prob)
  
  collection <- new_cia_collection(collection)
  
  return(collection)
}

#' @export
CollectUniqueObjects.cia_chains <- function(x) {
  
  collections <- list()
  n_chains <- length(x)
  for (i in 1:n_chains) {
    collections[[i]] <- CollectUniqueObjects(x[[i]])
    collections[[i]] <- new_cia_collection(collections[[i]])
  }
  
  collections <- new_cia_collections(collections)
  
  return(collections)
}

#' Constructor for a cia_chains collection.
#' 
#' @param x A list corresponding to a single mcmc chain.
#' @returns cia_chain A cia_chain object.
#' 
#' @noRd
new_cia_collections <- function(x) {
  
  stopifnot(is.list(x))
  cia_collections <- structure(x, class = 'cia_collections')
  
  return(cia_collections)
}

#' Constructor for a cia_chain collection.
#' 
#' @param x A list corresponding to a single mcmc chain.
#' @returns cia_chain A cia_chain object.
#' 
#' @noRd
new_cia_collection <- function(x) {
  
  stopifnot(is.list(x))
  cia_collection <- structure(x, class = 'cia_collection')
  
  return(cia_collection)
}
