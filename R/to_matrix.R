#' Convert bnlearn network object to an adjacency matrix. 
#' 
#' @examples
#' toMatrix(bnlearn::empty.graph(LETTERS[1:6]))
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
