#' Convert DAG to CPDAG.
#' 
#' @param x A matrix, dagmc_chain, or dagmc_chains object. When it is a chain(s)
#' object the state must be an adjacency matrix.
#' @returns x Returns same object type converted to a CPDAG.
#' 
#' @export
DAGtoCPDAG <- function(x) UseMethod('DAGtoCPDAG')

#' @export
DAGtoCPDAG.matrix <- function(x) {
  
  cpdag_mat <- x |>
    toBNLearn() |>
    bnlearn::cpdag() |>
    toMatrix()
  
  return(cpdag_mat)
}

#' @export
DAGtoCPDAG.dagmc_chains <- function(x) {
  
  n_chains <- length(x)
  
  cl <- parallel::makeCluster(n_chains)
  doParallel::registerDoParallel(cl)
  i <- NULL
  chains <- foreach::foreach(i = 1:n_chains) %dopar% {
    DAGtoCPDAG(x[[i]])
  }
  parallel::stopCluster(cl)
  
  chains <- new_dagmc_chains(chains)
  
  return(chains)
}

#' @export
DAGtoCPDAG.dagmc_chain <- function(x) {
  
  x$state <- lapply(x$state, DAGtoCPDAG)
  
  return(x)
}
