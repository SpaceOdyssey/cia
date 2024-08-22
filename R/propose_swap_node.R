#' Swap node proposal.
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @noRd
SwapNode <- function(partitioned_nodes) {
  current_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
  
  proposed <- ProposeSwapNode(partitioned_nodes)
  partitioned_nodes <- proposed$partitioned_nodes
  rescore_nodes <- proposed$rescore_nodes
  
  new_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
  
  return(list(
    state = partitioned_nodes, 
    current_nbd = current_nbd, 
    new_nbd = new_nbd,
    rescore_nodes = rescore_nodes))
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
#' @noRd
ProposeSwapNode <- function(partitioned_nodes) {
  
  if (GetNumberOfPartitions(partitioned_nodes) > 1) {

    # Select node.
    n <- GetNumberOfNodes(partitioned_nodes)
    i_node <- sample.int(n, size = 1)
    node <- partitioned_nodes$node[i_node]
    node_element <- partitioned_nodes$partition[i_node]
    
    # Select node from another partition element.
    candidates <- partitioned_nodes$node[partitioned_nodes$partition != node_element]
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
#' @noRd
CalculateSwapNodeNeighbourhood <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  if (m > 1) {
    n <- GetNumberOfNodes(partitioned_nodes)
    
    nbd <- 0
    for (i in 1:m) {
      k_i <- sum(partitioned_nodes$partition == i)
      nbd <- nbd + k_i*(n - k_i)
    }
    nbd <- 0.5*nbd
  } else {
    nbd <- 1
  }
  
  return(nbd)
}
