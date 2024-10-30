#' Uniformly sample DAG
#' 
#' @param nodes A vector of node names.
#' 
#' @returns Adjacency matrix with elements designated as (parent, child).
#' 
#' @examples
#' UniformlySampleDAG(LETTERS[1:3])
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
#' @inheritParams UniformlySampleDAG
#' 
#' @returns An adjacency matrix with elements designated as (parent, child).
#' 
#' @examples
#' GetEmptyDAG(LETTERS[1:3])
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

