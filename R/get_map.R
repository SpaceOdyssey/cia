#' Get MAP DAG. TODO: I'm confused here too. Why should this specifically run on
#' the chain collection? Why not the chains or list of chains? What if I want to
#' check that the MAP is the same across chains? Why wouldn't this give back
#' the map per chain in that case? What if we have more complex 'states' that
#' aren't described by a single DAG (e.g., mixtures of DAGs).
#' 
#' Get the maximum a posteriori DAG.
#' 
#' @param x A collection of unique objects. See CollectUniqueObjects.
#' 
#' @returns dag A list with the adjacency matrix for the map and it's posterior 
#' probability. It is possible for it to return multiple DAGs. The list has
#' elements;
#' \itemize{
#'  \item dag: List of MAP DAGs.
#'  \item log_p: Vector with the log posterior probability for each DAG.
#' }
#' 
#' @export
GetMAP <- function(x) UseMethod('GetMAP')

#' @export
GetMAP.dagmc_collections <- function(x) {
  
  n_chains <- length(x)
  maps <- list()
  for (i in 1:n_chains) {
    maps[[i]] <- GetMAP(x[[i]])
  }
  
  return(maps)
}

#' @export
GetMAP.dagmc_collection <- function(x) {
  
  p_maps <- max(x$log_norm_state_score)
  ip_map <- which(x$log_norm_state_score == p_maps)
  states <- x$state[ip_map]
  
  maps <- list(
    state = states,
    log_p = exp(p_maps)
  )
  
  return(maps)
}
