#' Preprocessing for blacklisting.

#' Get the lowest pairwise scoring edges.
#' 
#' @description
#' Get the lowest c pairwise scoring edges represented as a blacklist matrix. 
#' This blacklisting procedure is motivated by Koller & Friedman (2003).
#' 
#' @param scorer A scorer object.
#' @param n_retain An integer representing the number of edges to retain.
#' 
#' @returns A boolean matrix of (parent, child) pairs for blacklisting.
#' 
#' @references 
#' 1. Koller D, Friedman N. Being Bayesian about network structure. A Bayesian 
#' approach to structure discovery in Bayesian networks. Mach Learn. 
#' 2003;50(1):95–125.
#' 
#' @export
GetLowestPairwiseScoringEdges <- function(scorer, n_retain) {
  
  blacklist <- scorer |>
    CalculatePairwiseScores() |>
    apply(2, function(x) x < sort(x, decreasing = TRUE)[n_retain]) 
  
  return(blacklist)
}

#' Calculate pairwise scores. 
#' 
#' The result can be used to blacklist low scoring edges.
#' 
#' @param scorer Scorer object.
#' 
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
    scorer$parameters$node <- parent 
    scorer$parameters$parents <- vector()
    parent_score <-  do.call(scorer$scorer, scorer$parameters)
    children <- setdiff(nodes, parent)
    for (child in children) {
      scorer$parameters$node <- child
      scorer$parameters$parents <- parent
      scores[parent, child] <- do.call(scorer$scorer, scorer$parameters) + parent_score
      
      other_children <- setdiff(children, child)
      for(oth_child in other_children) { 
        scorer$parameters$node <- oth_child
        scorer$parameters$parents <- vector()
        scores[parent, child] <- scores[parent, child] + do.call(scorer$scorer, scorer$parameters)
        }
    }
  }
  
  return(scores)
}
