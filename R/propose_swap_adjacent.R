#' Swap nodes from adjacent partition elements proposal.
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @export
SwapAdjacentNode <- function(partitioned_nodes) {
  current_nbd <- CalculateSwapAdjacentNodeNeighbourhood(partitioned_nodes)
  proposed <- ProposeSwapAdjacentNode(partitioned_nodes)
  new_nbd <- CalculateSwapAdjacentNodeNeighbourhood(proposed$partitioned_nodes)
  
  return(list(state = proposed$partitioned_nodes,
              current_nbd = current_nbd,
              new_nbd = new_nbd,
              rescore_nodes = proposed$rescore_nodes))
}

#' Propose that two nodes swap partition elements.
#' 
#' @examples 
#' dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' ProposeStayStill(partitioned_nodes)
#' 
#' @param partitioned_nodes labelled partition.
#' 
#' @return A proposed labelled partition.
#' 
#' @export
ProposeSwapAdjacentNode <- function(partitioned_nodes) {
  
  if (GetNumberOfPartitions(partitioned_nodes) > 1) {
    # Select node.
    n <- GetNumberOfNodes(partitioned_nodes)
    i_node <- sample.int(n, size = 1)
    node <- partitioned_nodes$node[i_node]
    node_element <- partitioned_nodes$partition[i_node]
    
    # Select node from adjacent partition element.
    adj_element <- c(node_element - 1, node_element + 1)
    candidates <- partitioned_nodes$node[partitioned_nodes$partition %in% adj_element]
    
    oth_node <- sample(candidates, 1)
    i_oth_node <- partitioned_nodes$node == oth_node
    oth_node_element <- partitioned_nodes$partition[i_oth_node]
    
    # Find nodes to rescore.
    if (node_element < oth_node_element) {
      start_rescore <- node_element + 1
      end_rescore <- oth_node_element + 1
      rescore_nodes <- node
    } else {
      start_rescore <- oth_node_element + 1
      end_rescore <- node_element + 1
      rescore_nodes <- oth_node
    }
    rescore_nodes <- c(rescore_nodes, GetPartitionNodes(partitioned_nodes, start_rescore:end_rescore))
    
    # Swap nodes.
    partitioned_nodes$partition[i_node] <- oth_node_element
    partitioned_nodes$partition[i_oth_node] <- node_element
    
    partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)
    
  } else {
    rescore_nodes <- NULL
  }
  
  return(list(partitioned_nodes = partitioned_nodes,
              rescore_nodes = rescore_nodes))
}

#' Calculate neighbourhood for swapping nodes.
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @export
CalculateSwapAdjacentNodeNeighbourhood <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  if (m > 1) {
    nbd <- 0
    for (i in 1:(m - 1)) {
      k_i <- sum(partitioned_nodes$partition == i)
      k_i_one <- sum(partitioned_nodes$partition == i + 1)
      nbd <- nbd + k_i*k_i_one
    }
  } else {
    nbd <- 1
  }
  
  return(nbd)
}
