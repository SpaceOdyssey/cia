#' Preprocessing for blacklisting.

#' Get the lowest pairwise scoring edges.
#' 
#' @description 
#' Get the lowest c pairwise scoring edges represented as a blacklist matrix.
#' 
#' @param scorer A scorer object.
#' @param c An integer representing the number of edges to retain.
#' 
#' @export
GetLowestScoringEdges <- function(scorer, c) {
  
  blacklist <- scorer %>%
    CalculatePairwiseScores %>% 
    apply(2, function(x) x < sort(x, decreasing = TRUE)[c]) 
  
  return(blacklist)
}

#' Calculate pairwise scores. 
#' 
#' The result can be used to blacklist low scoring edges.
#' 
#' @param scorer Scorer object.
#' @returns A matrix of (parent, child) scores. The diagonal represents the 
#' 
#' @noRd
CalculatePairwiseScores <- function(scorer) {
  
  nodes <- names(scorer$parameters$data)
  scores <- matrix(NA, 
                   nrow = length(nodes), 
                   ncol = length(nodes), 
                   dimnames = list(nodes, nodes))
  
  for (parent in nodes) {
    children <- setdiff(nodes, parent)
    for (child in children) {
      scorer$parameters$node <- child
      scorer$parameters$parents <- parent
      scores[parent, child] <- do.call(scorer$scorer, scorer$parameters)
    }
  }
  
  return(scores)
}
