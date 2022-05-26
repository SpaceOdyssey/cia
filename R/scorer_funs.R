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
#' @return scorer_table A list of log_scores for each combination in parent_combinations.
#' 
#' @export
ScoreTableNode <- function(node, potential_parents, scorer) {
  
  scorer$parameters$node <- node
  
  if (is.null(potential_parents)) {
    scorer$parameters$parents <- vector()
    log_node_parent_scores <- do.call(scorer$scorer, scorer$parameters)
    parent_combinations <- list(NULL)
  } else {
    parent_combinations <- GetParentCombinations(potential_parents)
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
#' @param node The node name.
#' @param potential_parents A vector of potential parents that need to be iterated over.
#' @param scorer A scorer object.
#' 
#' @return log_node_score The log of the node score.
#' 
#' @export
ScoreNode <- function(node, potential_parents, scorer) {
  
  score_table <- ScoreTableNode(node, potential_parents, scorer)
  log_node_score <- LogSumExp(score_table$log_scores)
  
  return(log_node_score)
}

#' Score labelled partition by adding the log scores for each node.
#' 
#' @examples
#' ScoreLabelledPartition(partitioned_nodes, scorer_1)
#' ScoreLabelledPartition(partitioned_nodes, scorer_2)
#' 
#' @param node The node name.
#' @param potential_parents A vector of potential parents that need to be iterated over.
#' @param scorer A scorer object.
#' 
#' @return log_node_score The log of the node score.
#' 
#' @export
ScoreLabelledPartition <- function(partitioned_nodes, scorer) {
  
  log_partition_score <- 0.0
  for (node in partitioned_nodes$node) {
    potential_parents <- GetLowerPartitionedNodes(node, partitioned_nodes)
    log_node_score <- ScoreNode(node, potential_parents, scorer)
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
#' @param old_partitioned_nodes A labelled partition.
#' @param new_partitioned_nodes A labelled partition.
#' 
#' @return changed_nodes A vector of changed nodes.
#' 
#' @export
FindChangedNodes <- function(old_partitioned_nodes, new_partitioned_nodes) {
  
  changed_nodes <- c()
  for (node in old_partitioned_nodes$node) {
    potential_parents <- GetLowerPartitionedNodes(node, old_partitioned_nodes)
    new_potential_parents <- GetLowerPartitionedNodes(node, new_partitioned_nodes)
    
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
#' @return changed_nodes A vector of changed nodes.
#' 
#' @export
ScoreDiff <- function(old_partitioned_nodes, new_partitioned_nodes, scorer) {
  
  changed_nodes <- FindChangedNodes(old_partitioned_nodes, new_partitioned_nodes)
  log_score_diff <- 0.0
  for (node in changed_nodes) {
    potential_parents <- GetLowerPartitionedNodes(node, old_partitioned_nodes)
    log_score_node <- ScoreNode(node, potential_parents, scorer)
    
    new_potential_parents <- GetLowerPartitionedNodes(node, new_partitioned_nodes)
    new_log_score_node <- ScoreNode(node, new_potential_parents, scorer)
    
    log_score_diff <- log_score_diff + new_log_score_node - log_score_node
  }
  
  return(log_score_diff)
}

#' Calculate neighbourhood.
#' 
#' @export
CalculateNeighbourhood <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  ordered_partition <- GetOrderedPartition(partitioned_nodes)
  
  node_combinations <- 0
  for (i in 1:m) {
    # Get number of combinations of selections for a given partition.
    k_i <- ordered_partition$frequency[i]
    if (k_i == 1) {
      node_combinations <- node_combinations + 1
    } else {
      for (c in 1:(k_i - 1)) {
        node_combinations <- node_combinations + choose(k_i, c)
      }
    }
  }
  
  return(m - 1 + node_combinations)
}

# Scoring utilities.

#' Log-Sum-Exponential calculation using the trick that limits underflow issues.
#' 
#' @param x A vector of numeric.
#' @return lse The Log-Sum-Exponential of x.
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
#' @param node A node name.
#' @param partitioned_nodes A labelled partition.
#' 
#' @return node_partition A node's partition element number.
#' 
#' @export
GetNodePartition <- function(node, partitioned_nodes) {
  node_partition <- partitioned_nodes[partitioned_nodes$node == node, 'partition']
  
  return(node_partition)
}

#' Get potential parents of a node.
#' 
#' @param node A node name.
#' @param partitioned_nodes A labelled partition.
#' 
#' @return potential_parents A vector of potential parent names.
#' 
#' @export
GetLowerPartitionedNodes <- function(node, partitioned_nodes) {
  # Get higher-order nodes (i.e. find potential parents).
  node_partition <- GetNodePartition(node, partitioned_nodes)
  potential_parents <- partitioned_nodes[partitioned_nodes$partition < node_partition, 'node']
  
  if (identical(potential_parents, character(0)))
    potential_parents <- NULL
  
  return(potential_parents)
}

#' Get all parent combinations of nodes.
#' 
#' @param parents A vector of potential parent names.
#' 
#' @return parent_combinations A list of parent combinations.
#' 
#' @export
GetParentCombinations <- function(parents) {
  if (is.null(parents))
    return(parents)
  
  parents <- parents[order(parents)]
  
  parent_combinations <- lapply(
    1:length(parents),
    function(y) combn(parents, y, simplify = FALSE)
    ) %>%
    unlist(recursive = FALSE)
  
  return(parent_combinations)
}

#' Derived a labelled partition.
#'
#' This partitions nodes into levels of outpoints as explained in Section 4.1 of
#' Kuipers & Moffa 2015. This takes an adjacency matrix and returns a data.frame
#' of (partition, node) pairs
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
#' @export
OrderPartitionedNodes <- function(partitioned_nodes) {
  ord <- order(partitioned_nodes$partition, partitioned_nodes$node)
  partitioned_nodes <- partitioned_nodes[ord, ]

  return(partitioned_nodes)
}

#' Get ordered labelled partition.
#' 
#' Calculate the ordered partition. Labelled as lamba in Kuipers & Moffa (2015).
#'
#' @export
GetOrderedPartition <- function(partitioned_nodes) {
  
  ordered_partition <- table(partitioned_nodes$partition) %>%
    as.data.frame() %>%
    setNames(nm = c('partition', 'frequency'))
  
  return(ordered_partition)
}

