#' A transition object.

#' A one-step implementation of partition MCMC.
#' 
#' @examples 
#' PartitionMCMC(partitioned_nodes, proposal = ProposePartitionSplitJoin, scorer = scorer_1)
#' PartitionMCMC(partitioned_nodes, proposal = ProposePartitionSplitJoin, scorer = scorer_2)
#' 
#' @param current_state A labelled partition.
#' @param proposal A proposal function.
#' @param scorer A scorer object.
#'
#' @returns A new state.
#' 
#' @export
PartitionMCMC <- function(current_state, proposal, scorer) {
  
  proposed <- proposal(current_state$state)

  new_state <- proposed$state
  log_score_diff <- ScoreDiff(current_state$state, new_state, scorer)
  
  log_r <- log(proposed$current_nbd) - log(proposed$new_nbd) + log_score_diff
  if (AcceptProposal(log_r)) {
    current_state$state <- new_state
    current_state$log_score <- current_state$log_score + log_score_diff
  }
  
  return(current_state)
}

#' Sample a DAG from a labelled partition.
#' 
#' @examples 
#' SampleDAGFromLabelledPartition(partitioned_nodes, scorer = scorer_1)
#' 
#' @param partitioned_nodes A labelled partition.
#' @param scorer A scorer object.
#' 
#' @returns A data.frame of (node, partition element) pairs.
#' 
#' @export
SampleDAGFromLabelledPartition <- function(partitioned_nodes, scorer) {
  
  names <- sort(partitioned_nodes$node)
  dag <- matrix(
    0, 
    nrow = nrow(partitioned_nodes), ncol = nrow(partitioned_nodes), 
    dimnames = list(names, names)
  )
  
  for (node in partitioned_nodes$node) {
    
    # These next two lines are repeated several times through my code. Need
    # to make a function.
    potential_parents <- GetLowerPartitionedNodes(node, partitioned_nodes)
    score_table <- ScoreTableNode(node, potential_parents, scorer)
    
    # Normalise score table.
    log_z <- LogSumExp(score_table$log_scores)
    normalised_score_table <- score_table$log_scores - log_z
    
    # Create cumulative distribution function.
    ordered_log_scores <- order(normalised_score_table)
    normalised_score_table[ordered_log_scores]
    cdf_log_p <- c()
    for (i in 1:length(score_table$log_scores)) {
      cdf_tmp <- LogSumExp(normalised_score_table[ordered_log_scores[1:i]])
      cdf_log_p <- c(cdf_log_p, cdf_tmp)
    }
    
    # Select parents using the cumulative distribution.
    log_alpha <- log(runif(1))
    ordered_parents_i <- min(which(log_alpha < cdf_log_p))
    parent_combination_i <- ordered_log_scores[ordered_parents_i]
    parents <- score_table$parent_combinations[[parent_combination_i]]
    
    # Add (node, parents) to DAG.
    dag[parents, node] <- 1
  }
  
  return(dag)
}

# Utilities.

#' Metropolis-Hastings acceptance.
#' 
#' @param log_r Log of Metropolis-Hastings ratio.
#' @return accept Boolean.
#' 
#' @noRd
AcceptProposal <- function(log_r) {
  log_p <- min(0.0, log_r)
  log_alpha <- log(runif(1))
  if (log_alpha < log_p) {
    accept <- TRUE
  } else {
    accept <- FALSE
  }
  
  return(accept)
}