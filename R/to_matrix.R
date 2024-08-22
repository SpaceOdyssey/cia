#' Convert a DAG object from other libraries to an adjacency matrix. 
#' 
#' @param network A bnlearn or igraph object.
#' 
#' @examples
#' toMatrix(bnlearn::empty.graph(LETTERS[1:6]))
#' toMatrix(igraph::sample_k_regular(10, 2))
#' 
#' @param network A network object from bnlearn or igraph. 
#' @returns An adjacency matrix representation of network.
#' 
#' @export
toMatrix <- function(network) UseMethod('toMatrix') 

#' @export
toMatrix.bn <- function(network) {
  mat <- network |>
    bnlearn::as.igraph() |>
    toMatrix()
  
  return(mat)
}

#' @export
toMatrix.igraph <- function(network) {
  mat <- network |>
    igraph::as_adjacency_matrix() |>
    as.matrix()
  
  return(mat)
}

#' @export
toMatrix.matrix <- function(network) {
  return(network)
}
