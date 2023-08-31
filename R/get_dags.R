#' Uniformly sample DAG given a set of nodes.
#' 
#' @param nodes A vector of node names.
#' @return Adjacency matrix with elements designated as (parent, child).
#' 
#' @export
UniformlySampleDAG <- function(nodes) {
  
  dag <- nodes |>
    bnlearn::random.graph(method = 'melancon') |>
    BNLearnToMatrix()
  
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

#' Convert bnlearn network object to an adjacency matrix. 
#' 
#' @examples 
#' BNLearnToMatrix(bnlearn::empty.graph(LETTERS[1:6]))
#' 
#' @param network A bnlearn network object. 
#' @returns An adjacency matrix representation of network.
#' 
#' @noRd
BNLearnToMatrix <- function(network) {
  
  mat <- network |>
    bnlearn::as.igraph() |>
    igraph::as_adjacency_matrix() |>
    as.matrix()
  
  return(mat)
}
