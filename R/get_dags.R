#' Uniformly sample DAG given a set of nodes.
#' 
#' @param nodes A vector of node names.
#' @return Adjacency matrix with elements designated as (parent, child).
#' 
#' @export
UniformlySampleDAG <- function(nodes) {
  
  dag <- nodes |>
    bnlearn::random.graph(method = 'melancon') |>
    toMatrix()
  
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
    0L,
    ncol = length(nodes), nrow = length(nodes), 
    dimnames = list(nodes, nodes)
    )
    
  return(dag)
}

