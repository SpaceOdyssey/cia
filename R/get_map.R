#' Get the maximum a posteriori state.
#' 
#' @param x A collection of unique objects. See CollectUniqueObjects.
#' 
#' @returns maps A list with the adjacency matrix for the map and it's posterior 
#' probability. It is possible for it to return multiple DAGs. The list has
#' elements;
#' \itemize{
#'  \item state: List of MAP DAGs.
#'  \item log_p: Numeric vector with the log posterior probability for each state.
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
  
  maps <- list(
    state = x$state[ip_map],
    log_p = exp(p_maps),
    log_state_score = x$log_state_score[ip_map],
    log_norm_state_score = x$log_norm_state_score[ip_map]
  )
  
  return(maps)
}
