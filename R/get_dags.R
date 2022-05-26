#' Uniformly sample DAG given a set of nodes.
#'
#' @param nodes A vector of node names.
#' @return An adjacency matrix with elements designated as (parent, child).
#' 
#' @export
UniformlySampleDAG <- function(nodes) {
  
  dag <- bnlearn::random.graph(nodes, method = 'melancon') %>%
    bnlearn::as.igraph() %>%
    igraph::as_adjacency_matrix() %>%
    as.matrix
  
  return(dag)
}

#' Get an empty DAG given a set of nodes.
#'
#' @param nodes A vector of node names.
#' @return An adjacency matrix with elements designated as (parent, child).
#' 
#' @export
GetEmptyDAG <- function(nodes) {
  
  dag <- matrix(
    0, ncol = length(nodes), nrow = length(nodes), 
    dimnames = list(nodes, nodes)
    )
    
  return(dag)
}
