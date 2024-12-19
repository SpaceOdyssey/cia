#' Preprocessing for blacklisting

#' Get the lowest pairwise scoring edges.
#' 
#' @description
#' Get the lowest pairwise scoring edges represented as a blacklist matrix. 
#' This blacklisting procedure is motivated by Koller & Friedman (2003). This
#' is rarely used now as we found that it blacklists edges that have significant
#' dependencies but are not in the top \eqn{n} edges. We prefer 
#' the GetIncrementalScoringEdges method.
#' 
#' @param scorer A scorer object.
#' @param n_retain An integer representing the number of edges to retain.
#' 
#' @returns A boolean matrix of (parent, child) pairs for blacklisting.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data
#'   )
#'   
#' blacklist <- GetLowestPairwiseScoringEdges(scorer, 3)
#' 
#' blacklist_scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data,
#'   blacklist = blacklist,
#'   cache = TRUE
#'   )
#'
#' init_state <- InitPartition(colnames(data), blacklist_scorer)
#' 
#' results <- SampleChains(10, init_state, PartitionMCMC(), blacklist_scorer)
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
#' The result can be used to blacklist low scoring edges. This is looking at
#' marginal effects.
#' 
#' @param scorer Scorer object.
#' 
#' @returns A matrix of (parent, child) scores.
#' 
#' @noRd
CalculatePairwiseScores <- function(scorer) {
  
  nodes <- names(scorer$parameters$data)
  scores <- matrix(TRUE,
                   nrow = length(nodes),
                   ncol = length(nodes), 
                   dimnames = list(nodes, nodes))
  
  for (parent in nodes) {
    scorer$parameters$node <- parent 
    scorer$parameters$parents <- vector()
    parent_score <- do.call(scorer$scorer, scorer$parameters)
    children <- setdiff(nodes, parent)
    for (child in children) {
      scorer$parameters$node <- child
      scorer$parameters$parents <- parent
      scores[parent, child] <- do.call(scorer$scorer, scorer$parameters) + parent_score
      
      other_children <- setdiff(children, child)
      for (oth_child in other_children) { 
        scorer$parameters$node <- oth_child
        scorer$parameters$parents <- vector()
        scores[parent, child] <- scores[parent, child] + do.call(scorer$scorer, scorer$parameters)
        }
    }
  }
  
  return(scores)
}

#' Get incremental edges
#' 
#' Get edges that do not incrementally improve the score over an empty DAG 
#' greater than a cutoff. In detail, this returns the edges where a graph
#' with the edge \eqn{E} given by \eqn{g_E} such that 
#' Score(g_E) - Score(g_empty) < cutoff. Assuming that the scorer returns the 
#' log of the marginalised posterior, then the cutoff corresponds to the log of 
#' the Bayes Factor. The output can be used as a blacklist.
#' 
#' @param scorer A scorer object.
#' @param cutoff A score cutoff. The score cutoff is equal to the log
#' of the Bayes Factor between the two models.
#' 
#' @returns A Boolean matrix of (parent, child) pairs for blacklisting.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data
#'   )
#'   
#' blacklist <- GetIncrementalScoringEdges(scorer, cutoff = -10.0)
#' 
#' blacklist_scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data,
#'   blacklist = blacklist,
#'   cache = TRUE
#'   )
#'
#' # Randomly sample a starting DAG consistent with the blacklist. Then
#' # convert to a partition.
#' init_state <- InitPartition(colnames(data), blacklist_scorer)
#' 
#' results <- SampleChains(10, init_state, PartitionMCMC(), blacklist_scorer)
#'
#'@export
GetIncrementalScoringEdges <- function(scorer, cutoff = 0.0){
  
  inc_score <- CalculateIncrementalPairwiseScores(scorer)
  blacklist <- inc_score < cutoff
  
  return(blacklist)
}



#' Get the score of the empty DAG
#' 
#'@param scorer A scorer object.
#' 
#'@returns log of empty DAG.
#'
#'@noRd
ScoreEmptyDAG <- function(scorer) {
  
  nodes <- colnames(scorer$parameters$data)
  empty_dag <- matrix(0L, nrow = ncol(scorer$parameters$data), ncol = ncol(scorer$parameters$data),
                      dimnames = list(nodes, nodes))
  log_score <- ScoreDAG(empty_dag, scorer)
  
  return(log_score)
}


#' Get the positive incremental pairwise scoring edges
#' 
#' @description Get the positive incremental pairwise scoring edges represented as a blacklist matrix
#' 
#' 
#'@param scorer A scorer object.
#' 
#'@returns A matrix of the difference between Pairwise Score and EmptyDAG Score
#'
#'@noRd
CalculateIncrementalPairwiseScores <- function(scorer) {
  
  inc_score <- CalculatePairwiseScores(scorer) - ScoreEmptyDAG(scorer)
  
  return(inc_score)
}



#' Get partially incremental scoring edges. This is an unused function.
#' 
#' @description Get the positive incremental scoring edges after conditioning
#' on all other variables.
#' 
#' @param scorer A scorer object.
#' @param cutoff A cutoff value for the blacklist. Less than this value is 
#' blacklisted.
#' 
#' @noRd
GetPartiallyIncrementalEdges <- function(scorer, cutoff = 0.0) {
  
  part_score <- CalculatePartiallyIncrementalScores(scorer)
  blacklist <- part_score < cutoff
  part_score[is.na(part_score)] <- TRUE
  
  return(blacklist) 
}

#' Calculate partially incremental scoring edges.
#' 
#' @noRd
CalculatePartiallyIncrementalScores <- function(scorer) {
  
  nodes <- names(scorer$parameters$data)
  part_scores <- matrix(NA, 
                        nrow = length(nodes),
                        ncol = length(nodes),
                        dimnames = list(nodes, nodes))
  
  for (child in nodes) {
    retrict_pas <- GetRestrictedParents(child, scorer$blacklist)
    poss_pas <- setdiff(nodes, c(child, retrict_pas))
    for (poss_pa in poss_pas) {
      cond_nodes <- setdiff(poss_pas, poss_pa)
      
      # Empty conditional node scores.
      cond_score_pa <- 0.0
      for (cond_node in cond_nodes) {
        scorer$parameters$node <- cond_node
        scorer$parameters$parents <- vector()
        cond_score_pa <- cond_score_pa + do.call(scorer$scorer, scorer$parameters)
      }
      
      # Calculate score of total graph wrt all conditional nodes.
      scorer$parameters$node <- child
      scorer$parameters$parents <- cond_nodes
      cond_score <- do.call(scorer$scorer, scorer$parameters) + cond_score_pa
      
      # Calculate score of total graph with all conditional nodes + poss parent.
      scorer$parameters$node <- child
      scorer$parameters$parents <- poss_pas
      poss_pa_score <- do.call(scorer$scorer, scorer$parameters) + cond_score_pa      
      
      part_scores[poss_pa, child] <- poss_pa_score - cond_score
    }
  }
  
  return(part_scores)
}
