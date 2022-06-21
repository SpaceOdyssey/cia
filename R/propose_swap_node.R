#' Swap node proposal.
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @export
SwapNode <- function(partitioned_nodes) {
  current_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
  
  partitioned_nodes <- ProposeSwapNode(partitioned_nodes)
  new_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
  
  return(list(
    state = partitioned_nodes, 
    current_nbd = current_nbd, 
    new_nbd = new_nbd))
}

#' Propose that two nodes swap partition elements.
#' 
#' @examples 
#' ProposeStayStill(partitioned_nodes)
#' 
#' @param partitioned_nodes labelled partition.
#' 
#' @return A proposed labelled partition.
#' 
#' @export
ProposeSwapNode <- function(partitioned_nodes) {
  
  if (GetNumberOfPartitions(partitioned_nodes) > 1) {

    # Select node.
    n <- nrow(partitioned_nodes)
    i_node <- sample.int(n, size = 1)
    node_element <- partitioned_nodes[i_node, 'partition'] 
    
    # Select node from another partition element.
    candidates <- partitioned_nodes[partitioned_nodes$partition != node_element, 'node']
    oth_node <- sample(candidates, 1)
    i_oth_node <- partitioned_nodes$node == oth_node
    oth_node_element <- partitioned_nodes[i_oth_node, 'partition']
    
    # Swap nodes.
    partitioned_nodes[i_node, 'partition'] <- oth_node_element
    partitioned_nodes[i_oth_node, 'partition'] <- node_element
    
    partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)
  }
  
  return(partitioned_nodes)
}

#' Calculate neighbourhood for swapping nodes.
#' 
#' @param partitioned_nodes A labelled partition.
#' 
#' @export
CalculateSwapNodeNeighbourhood <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  if (m > 1) {
    n <- nrow(partitioned_nodes)
    
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
