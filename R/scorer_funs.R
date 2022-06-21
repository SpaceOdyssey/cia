#' Calculate score tables for (node, parents) combinations. 
#'
#' @examples
#' ScoreTableNode('A', c('B', 'C'), scorer_1)
#' ScoreTableNode('A', c('B', 'C'), scorer_2)
#' ScoreTableNode('A', c(), scorer_1)
#' ScoreTableNode('A', NULL, scorer_1)
#' 
#' @param node The node name.
#' @param potential_parents A vector of potential parents that need to be iterated over.
#' @param scorer A scorer object.
#'
#' @return List of log_scores for each combination in parent_combinations.
#' 
#' @export
ScoreTableNode <- function(partitioned_nodes, node, scorer) {
  
  scorer$parameters$node <- node
  parent_combinations <- GetParentCombinations(partitioned_nodes, node)
  
  if (is.null(parent_combinations)) {
    scorer$parameters$parents <- vector()
    log_node_parent_scores <- do.call(scorer$scorer, scorer$parameters)
    parent_combinations <- list(NULL)
  } else {
    log_node_parent_scores <- c()
    for (parents in parent_combinations) {
      scorer$parameters$parents <- parents
      log_parent_comb_score <- do.call(scorer$scorer, scorer$parameters)
      log_node_parent_scores <- c(log_node_parent_scores, log_parent_comb_score)
    }
  }
  
  score_table <- list(
    log_scores = log_node_parent_scores,
    parent_combinations = parent_combinations
  )
  
  return(score_table)
}

#' Score node by marginalising over parent combinations.
#' 
#' @examples 
#' ScoreNode('A', c('B', 'C'), scorer_1)
#' ScoreNode('A', c('B', 'C'), scorer_2)
#' 
#' @param partitioned_nodes Labelled partition.
#' @param node The node name.
#' @param scorer A scorer object.
#' 
#' @return Log of the node score.
#' 
#' @export
ScoreNode <- function(partitioned_nodes, node, scorer) {
  
  score_table <- ScoreTableNode(partitioned_nodes, node, scorer)
  log_node_score <- LogSumExp(score_table$log_scores)
  
  return(log_node_score)
}

#' Score labelled partition by adding the log scores for each node.
#' 
#' @examples
#' ScoreLabelledPartition(partitioned_nodes, scorer_1)
#' ScoreLabelledPartition(partitioned_nodes, scorer_2)
#' 
#' @param partitioned_nodes Labelled partition.
#' @param node The node name.
#' @param scorer A scorer object.
#' 
#' @return Log of the node score.
#' 
#' @export
ScoreLabelledPartition <- function(partitioned_nodes, scorer) {
  
  log_partition_score <- 0.0
  for (node in partitioned_nodes$node) {
    log_node_score <- ScoreNode(partitioned_nodes, node, scorer)
    log_partition_score <- log_partition_score + log_node_score
  }
  
  return(log_partition_score)
}

#' Find nodes with changed parent combinations between different labelled 
#' partitions.
#' 
#' @examples 
#' changed_nodes <- FindChangedNodes(partitioned_nodes, new_partitioned_nodes)
#' 
#' @param old_partitioned_nodes Labelled partition.
#' @param new_partitioned_nodes Labelled partition.
#' 
#' @return Vector of changed nodes.
#' 
#' @export
FindChangedNodes <- function(old_partitioned_nodes, new_partitioned_nodes) {
  
  changed_nodes <- c()
  for (node in old_partitioned_nodes$node) {
    potential_parents <- GetParentCombinations(old_partitioned_nodes, node)
    new_potential_parents <- GetParentCombinations(new_partitioned_nodes, node)
    
    # TODO: This shouldn't check the whole set. Instead, 1) check direct parents, 2) check indirect parents.
    if (!setequal(potential_parents, new_potential_parents)) {
      changed_nodes <- c(changed_nodes, node)
    }
  }
  
  return(changed_nodes)
}


#' Calculate the difference in log scores between two labelled partitions. 
#' 
#' @examples 
#' ScoreDiff(partitioned_nodes, new_partitioned_nodes, scorer = scorer_1)
#' ScoreDiff(partitioned_nodes, new_partitioned_nodes, scorer = scorer_2)
#' 
#' @param old_partitioned_nodes A labelled partition.
#' @param new_partitioned_nodes A labelled partition.
#' @param scorer A scorer object.
#' 
#' @return Log of score difference between two labelled partitions.
#' 
#' @export
ScoreDiff <- function(old_partitioned_nodes, new_partitioned_nodes, scorer) {
  
  changed_nodes <- FindChangedNodes(old_partitioned_nodes, new_partitioned_nodes)
  log_score_diff <- 0.0
  for (node in changed_nodes) {
    old_log_score_node <- ScoreNode(old_partitioned_nodes, node, scorer)
    new_log_score_node <- ScoreNode(new_partitioned_nodes, node, scorer)
    
    log_score_diff <- log_score_diff + new_log_score_node - old_log_score_node
  }
  
  return(log_score_diff)
}

#' Score DAG.
#' 
#' @param dag Adjacency matrix of (parent, child) entries with 1 denoting an 
#' edge and 0 otherwise.
#' 
#' @returns Log of DAG score.
#'
#' @export
ScoreDAG <- function(dag, scorer) {
  
  log_score <- 0.0
  for (child in colnames(dag)) {
    scorer$parameters$node <- child
    
    pa_bool <- dag[, child] == 1
    if (sum(pa_bool) > 0) {
      scorer$parameters$parents <- names(which(pa_bool))
    } else {
      scorer$parameters$parents <- vector()
    }
    
    log_score_node <- do.call(scorer$scorer, scorer$parameters)
    log_score <- log_score + log_score_node
  }
  
  return(log_score)
}

# Scoring utilities.

#' Log-Sum-Exponential calculation using the trick that limits underflow issues.
#' 
#' @param x A vector of numeric.
#' @return Log-Sum-Exponential (LSE) of x.
#' 
#' @export
LogSumExp <- function(x) {
  x_max <- max(x)
  lse <- x_max + log(sum(exp(x - x_max)))
  
  return(lse)
}

#' Get number of partitions.
#' 
#' Calculate the number of partitions for a given labelled partition. This is 
#' 'm' in Kuipers & Moffa (2015).
#' 
#' @param partitoned_nodes
#' 
#' @export
GetNumberOfPartitions <- function(partitioned_nodes) {
  return(max(partitioned_nodes$partition))
}

#' Get a node's partition element number.
#' 
#' @param node Node name.
#' @param partitioned_nodes Labelled partition.
#' 
#' @return Node's partition element number.
#' 
#' @export
GetNodePartition <- function(partitioned_nodes, node) {
  node_partition <- partitioned_nodes[partitioned_nodes$node == node, 'partition']
  
  return(node_partition)
}

#' Get parent combinations for a given node.
#' 
#' @param partitioned_nodes Labelled partition.
#' @param node A node name.
#' 
#' @return List of parent combinations.
#' 
#' @export
GetParentCombinations <- function(partitioned_nodes, node) {
  
  node_el <- GetNodePartition(partitioned_nodes, node)
  
  if (node_el == 1) {
    parent_combinations <- NULL
  } else if (node_el == 2) {
    direct_pas <- partitioned_nodes[partitioned_nodes$partition == node_el - 1, 'node']
    direct_pa_coms <- GetNodeCombinations(direct_pas)
    
    parent_combinations <- direct_pa_coms
  } else {
  
    direct_pas <- partitioned_nodes[partitioned_nodes$partition == node_el - 1, 'node']
    direct_pa_coms <- GetNodeCombinations(direct_pas)
    
    indirect_pas <- partitioned_nodes[partitioned_nodes$partition < node_el - 1, 'node']
    indirect_pa_coms <- GetNodeCombinations(indirect_pas)
  
    parent_combinations <- direct_pa_coms
    i <- 1 + length(direct_pa_coms)
    for (direct_pa_com in direct_pa_coms) {
      for (indirect_pa_com in indirect_pa_coms) {
        parent_combinations[[i]] <- c(direct_pa_com, indirect_pa_com)
        i <- i + 1
      }
    }
  }
  
  return(parent_combinations)
}

#' Get all combinations of nodes.
#' 
#' @param parents A vector of nodes.
#' 
#' @return List of parent combinations.
#' 
#' @export
GetNodeCombinations <- function(nodes) {
  if (is.null(nodes))
    return(nodes)
  
  nodes <- nodes[order(nodes)]
  
  node_combinations <- lapply(
      1:length(nodes),
      function(y) combn(nodes, y, simplify = FALSE)
    ) %>%
    unlist(recursive = FALSE)
  
  return(node_combinations)
}

#' Map DAG to a labelled partition.
#'
#' This partitions nodes into levels of outpoints as explained in Section 4.1 of
#' Kuipers & Moffa 2015. This takes an adjacency matrix and returns a data.frame
#' of (partition, node) pairs
#'
#' @param adjacency Adjacency matrix.
#' @returns Labelled partition for the given adjacency matrix.
#'
#' @export
GetPartitionedNodesFromAdjacencyMatrix <- function(adjacency) {
  remaining_nodes <- colnames(adjacency)
  partitions <- c()
  nodes <- c()
  i <- 1
  while (length(remaining_nodes) > 1) {
    outpoint_bool <- colSums(adjacency[remaining_nodes, remaining_nodes]) == 0

    nodes <- c(nodes, names(which(outpoint_bool)))
    partitions <- c(partitions, rep(i, sum(outpoint_bool)))

    remaining_nodes <- names(which(!outpoint_bool))
    i <- i + 1
  }

  if (length(remaining_nodes) == 1) {
    nodes <- c(nodes, remaining_nodes)
    partitions <- c(partitions, i)
  }

  partitioned_nodes <- data.frame(partition = partitions, node = nodes)
  partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)

  return(partitioned_nodes)
}

#' Order partitioned nodes.
#' 
#' @param partitioned_nodes Labelled partition.
#' @return Labelled partitioned in descending partition element order.
#' 
#' @export
OrderPartitionedNodes <- function(partitioned_nodes) {
  ord <- order(partitioned_nodes$partition, partitioned_nodes$node)
  partitioned_nodes <- partitioned_nodes[ord, ]

  return(partitioned_nodes)
}

#' Get ordered labelled partition.
#' 
#' Calculate the ordered partition. Denoted as lamba in Kuipers & Moffa (2015).
#'
#' @param partitioned_nodes Labelled partition.
#' @return Ordered partition. 
#'
#' @export
GetOrderedPartition <- function(partitioned_nodes) {
  
  ordered_partition <- table(partitioned_nodes$partition) %>%
    as.data.frame() %>%
    setNames(nm = c('partition', 'frequency'))
  
  return(ordered_partition)
}

