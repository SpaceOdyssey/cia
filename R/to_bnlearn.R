#' Convert to bnlearn object.
#' 
#' @param x Adjacency matrix.
#' @returns bn_obj A bn object.
#' 
#' @examples
#' adj <- UniformlySampleDAG(c('A', 'B', 'C'))
#' bn_obj <- toBNLearn(adj)
#' 
#' @export
toBNLearn <- function(x) UseMethod('toBNLearn')

#' @export
toBNLearn.default <- function(x) { 
  return(toBNLearn.matrix(x)) 
}

#' @export
toBNLearn.matrix <- function(x) {
  
  names <- colnames(x)
  bn_obj <- bnlearn::empty.graph(names)
  bnlearn::amat(bn_obj) <- x
  
  return(bn_obj)
}

#' @export
toBNLearn.bn <- function(x) {
  return(x)
}