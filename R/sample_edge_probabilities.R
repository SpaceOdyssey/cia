#' Sample edge probabilities using chains object.
#' 
#' @param x A chain(s) or collection object where states are DAGs.
#' 
#' @returns p_edge A posterior sample for the marginalised edge probabilities.
#' 
#' @export
SampleEdgeProbabilities <- function(x) {
  p_edge <- SamplePosteriorPredictiveChains(x, function(dag) return(as.vector(dag)))
  
  return(p_edge)
} 
