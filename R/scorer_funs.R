#' Calculate score tables for (node, parents) combinations. 
#'
#' @examples
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(names(data))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' 
#' scorer <- list(
#'   scorer = BNLearnScorer, 
#'   parameters = list(data = data)
#'   )
#'   
#' ScoreTableNode(partitioned_nodes, 'A', scorer)
#' 
#' @param partitioned_nodes Labelled partition.
#' @param node Name of node.
#' @param scorer Scorer object.
#'
#' @return List of log_scores for each combination in parent_combinations.
#' 
#' @export
ScoreTableNode <- function(partitioned_nodes, node, scorer) {
  
  scorer$parameters$node <- node
  parent_combinations <- GetParentCombinations(partitioned_nodes, node, scorer)

  if (is.null(parent_combinations)) {
    scorer$parameters$parents <- vector()
    log_node_parent_scores <- do.call(scorer$scorer, scorer$parameters)
    parent_combinations <- list(NULL)
  } else {
    log_node_parent_scores <- numeric(length(parent_combinations))
    for (i in 1:length(parent_combinations)) {
      scorer$parameters$parents <- parent_combinations[[i]]
      log_node_parent_scores[i] <- do.call(scorer$scorer, scorer$parameters)
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
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(names(data))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' 
#' scorer <- list(
#'   scorer = BNLearnScorer, 
#'   parameters = list(data = data)
#'   )
#'   
#' ScoreNode(partitioned_nodes, 'A', scorer)
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
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(names(data))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' 
#' scorer <- list(
#'   scorer = BNLearnScorer, 
#'   parameters = list(data = data)
#'   )
#' 
#' ScoreLabelledPartition(partitioned_nodes, scorer)
#' 
#' @param partitioned_nodes Labelled partition.
#' @param scorer Scorer object.
#' 
#' @return Log of the node score.
#' 
#' @export
ScoreLabelledPartition <- function(partitioned_nodes, scorer) {
  
  whitelist_obeyed <- CheckWhitelistObeyed(partitioned_nodes, scorer$whitelist)
  blacklist_obeyed <- CheckBlacklistObeyed(partitioned_nodes, scorer$blacklist)
  if (!whitelist_obeyed | !blacklist_obeyed)
    return(-Inf)
  
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
#' scorer = CreateScorer()
#' 
#' old_dag <- UniformlySampleDAG(LETTERS[1:5])
#' old_partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(old_dag)
#' 
#' new_dag <- UniformlySampleDAG(LETTERS[1:5])
#' new_partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(new_dag)
#' 
#' changed_nodes <- FindChangedNodes(old_partitioned_nodes, new_partitioned_nodes, scorer)
#' 
#' @param old_partitioned_nodes Labelled partition.
#' @param new_partitioned_nodes Labelled partition.
#' @param scorer Scorer object.
#' 
#' @return Vector of changed nodes.
#' 
#' @export
FindChangedNodes <- function(old_partitioned_nodes, new_partitioned_nodes, scorer) {
  
  changed_nodes <- c()
  for (node in old_partitioned_nodes$node) {
    potential_parents <- GetParentCombinations(old_partitioned_nodes, node, scorer)
    new_potential_parents <- GetParentCombinations(new_partitioned_nodes, node, scorer)

    if (!setequal(potential_parents, new_potential_parents)) {
      changed_nodes <- c(changed_nodes, node)
    }
  }
  
  return(changed_nodes)
}


#' Calculate the difference in log scores between two labelled partitions. 
#' 
#' @examples 
#' data <- bnlearn::learning.test
#' 
#' old_dag <- UniformlySampleDAG(names(data))
#' old_partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(old_dag)
#' 
#' new_dag <- UniformlySampleDAG(names(data))
#' new_partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(new_dag)
#'
#' scorer <- list(
#'   scorer = BNLearnScorer, 
#'   parameters = list(data = data)
#'   )
#' 
#' ScoreDiff(old_partitioned_nodes, new_partitioned_nodes, scorer = scorer)
#' 
#' @param old_partitioned_nodes A labelled partition.
#' @param new_partitioned_nodes A labelled partition.
#' @param scorer A scorer object.
#' 
#' @return Log of score difference between two labelled partitions.
#' 
#' @export
ScoreDiff <- function(old_partitioned_nodes, new_partitioned_nodes, scorer) {
  
  white_obeyed <- CheckWhitelistObeyed(new_partitioned_nodes, scorer$whitelist)
  black_obeyed <- CheckBlacklistObeyed(new_partitioned_nodes, scorer$blacklist)
  if (!white_obeyed | !black_obeyed)
    return(-Inf)
  
  changed_nodes <- FindChangedNodes(old_partitioned_nodes, 
                                    new_partitioned_nodes, 
                                    scorer)
  
  log_score_diff <- 0.0
  for (node in changed_nodes) {
    old_log_score_node <- ScoreNode(old_partitioned_nodes, node, scorer)
    new_log_score_node <- ScoreNode(new_partitioned_nodes, node, scorer)
    
    log_score_diff <- log_score_diff + new_log_score_node - old_log_score_node
  }
  
  return(log_score_diff)
}

#' Check whitelist is obeyed.
#' 
#' @param partitioned_nodes Labelled partition.
#' @param whitelist A data.frame of (parent, child) pairs representing edges 
#' that must be in the DAG.
#' @param nodes A vector of node names to check. Default is to check all 
#' child nodes in the whitelist.
#' 
#' @export
CheckWhitelistObeyed <- function(partitioned_nodes, whitelist = NULL, nodes = NULL) {
  
  if (is.null(whitelist))
    return(TRUE)
  
  if (is.null(nodes))
    nodes <- GetRestrictedNodes(whitelist)
  
  for (node in nodes) {
    node_el <- partitioned_nodes$partition[partitioned_nodes$node == node]
    
    if (node_el == 1)
      return(FALSE)
    
    whitelist_parents <- GetRestrictedParents(node, whitelist)
    lower_nodes <- partitioned_nodes$node[partitioned_nodes$partition < node_el]
    
    if (!all(whitelist_parents %in% lower_nodes))
      return(FALSE)
  }
  
  return(TRUE)
}


#' Check blacklist obeyed.
#' 
#' If an edge between two nodes is blacklisted in Partition MCMC the adjacent
#' partition element cannot be the only direct node for it's blacklisted child.
#' 
#' @param partitioned_nodes Labelled partition.
#' @param blacklist A data.frame of (parent, child) pairs representing edges 
#' that cannot be in the DAG.
#' @param nodes A vector of node names to check. Default is to check all 
#' child nodes in the blacklist.
#'
#' @export
CheckBlacklistObeyed <- function(partitioned_nodes, blacklist = NULL, 
                                 nodes = NULL) {
  
  if (is.null(blacklist))
    return(TRUE)
  
  if (is.null(nodes))
    nodes <- GetRestrictedNodes(blacklist)
  
  for (node in nodes) {
    node_el <- partitioned_nodes$partition[partitioned_nodes$node == node]
    
    if (node_el == 1)
      next
    
    blacklist_parents <- GetRestrictedParents(node, blacklist)
    lower_adj_nodes <- partitioned_nodes$node[partitioned_nodes$partition == node_el - 1]
    
    n_non_blacklisted <- length(setdiff(lower_adj_nodes, blacklist_parents))
    if (n_non_blacklisted == 0)
      return(FALSE)
  }
  
  return(TRUE)
}

#' Score DAG.
#' 
#' @param dag Adjacency matrix of (parent, child) entries with 1 denoting an 
#' edge and 0 otherwise.
#' @param scorer Scorer object.
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
#' @param partitioned_nodes Labelled partition.
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
  node_partition <- partitioned_nodes$partition[partitioned_nodes$node == node]
  
  return(node_partition)
}

#' Get parent combinations for a given node.
#' 
#' @param partitioned_nodes Labelled partition.
#' @param node Node name.
#' @param scorer A scorer object.
#' 
#' @return List of parent combinations.
#' 
#' @export
GetParentCombinations <- function(partitioned_nodes, node, scorer) {
  
  # If this function is exported I probably need to check if whitelist/blacklist are obeyed.
  
  node_el <- GetNodePartition(partitioned_nodes, node)
  
  if (node_el == 1)
    return(NULL)
  
  # Get white/black listed parent sets.
  whitelist_parents <- GetRestrictedParents(node, scorer$whitelist)
  n_whitelist <- length(whitelist_parents)
  
  blacklist_parents <- GetRestrictedParents(node, scorer$blacklist)
  
  # Get possible parents from adjacent element.
  direct_pas <- partitioned_nodes$node[partitioned_nodes$partition == node_el - 1]
  direct_pas <- direct_pas %>% 
    setdiff(whitelist_parents) %>%
    setdiff(blacklist_parents)
  
  # Add direct parent combinations.
  max_direct <- min(length(direct_pas), scorer$max_parents - n_whitelist)
  ls_direct_pa_coms <- lapply(
      1:max_direct, 
      function(x) arrangements::combinations(direct_pas, x, layout = 'list')
    ) 
  
  parent_combinations <- ls_direct_pa_coms %>%
    unlist(recursive = FALSE) %>%
    lapply(function(x) c(whitelist_parents, x))
  
  # Get possible parents from non-adjacent elements.
  if (node_el > 2) {
    indirect_pas <- partitioned_nodes$node[partitioned_nodes$partition < node_el - 1]
    indirect_pas <- indirect_pas %>%
      setdiff(whitelist_parents) %>%
      setdiff(blacklist_parents)
  
    # Get possible parents from non-adjacent elements.
    indirect_parent_combinations <- list()
    n <- 1
    max_direct_selected <- min(length(direct_pas), scorer$max_parents - n_whitelist)
    for (i in 1:max_direct_selected) {
      direct_pas_i <- ls_direct_pa_coms[[i]]
      
      max_indirect_selected <- min(length(indirect_pas), scorer$max_parents - n_whitelist - i)
      for (j in 0:max_indirect_selected) {
        indirect_pas_j <- arrangements::combinations(indirect_pas, j, layout = 'list')
        
        for (direct_pa_i in direct_pas_i) {
          for (indirect_pa_j in indirect_pas_j) {
            indirect_parent_combinations[[n]] <- c(whitelist_parents, direct_pa_i, indirect_pa_j)
            n <- n + 1
          }
        }
      }
    }
    
    parent_combinations <- c(parent_combinations, indirect_parent_combinations)
  }
  
  return(parent_combinations)
}

#' Get black or white listed parents.
#' 
#' @param node The name of the node to get white or black listed parents.
#' @param listed A black or white list.
#'
#' @export
GetRestrictedParents <- function(node, listed = NULL) {
  
  if (is.null(listed)) {
    parents <- c()
  } else {
    parents <- names(which(listed[, node]))
  }
  
  return(parents)
}

#' Get nodes that have restricted parents.
#'
#' @param list A black or white list.
#'
#' @export
GetRestrictedNodes <- function(list) {
  nodes <- names(which(colSums(list, na.rm = TRUE) > 0))
  
  return(nodes)
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
  row.names(partitioned_nodes) <- NULL

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
  
  tab <- tabulate(partitioned_nodes$partition)
  ordered_partition <- data.frame(partition = 1:length(tab), frequency = tab)
  
  return(ordered_partition)
}

#' Get number of nodes from labelled partition.
#' 
#' @noRd
GetNumberOfNodes <- function(partitioned_nodes) {
  return(dim.data.frame(partitioned_nodes)[1])
}
