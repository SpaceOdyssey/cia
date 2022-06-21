#' Node movement constructor.
#' 
#' @export
NodeMove <- function(partitioned_nodes, verbose = FALSE) {
  
  current_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
  
  partitioned_nodes <- ProposeNodeMove(partitioned_nodes, verbose)
  new_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
  
  return(list(
    state = partitioned_nodes,
    current_nbd = current_nbd, 
    new_nbd = new_nbd))
}

#' Propose individual node movement. 
#' 
#' This proposes that a single node selected uniformly can either:
#'   1) Move to any current partition element 
#'   2) Move to any gap between or at the ends of the elements.
#'   
#'  Any of these moves are possible and are selected uniformly. Note that
#'  this differs slightly from KP15 who remove the possibility of a 2-node
#'  partition element . This may slow the convergence on a per step basis but is 
#'  easier and more concise to implement.
#' 
#' @examples
#' ProposeNodeMovement(partitioned_nodes, verbose = TRUE)
#' 
#' @param partitioned_nodes A labelled partition.
#' @param verbose A flag to indicate that a single node is being moved.
#' 
#' @export
ProposeNodeMove <- function(partitioned_nodes, verbose = FALSE) {
  
  # Get number of current partitions.
  m <- GetNumberOfPartitions(partitioned_nodes)
  
  # Relabel current partitions into the new partition + gap space. Note that
  # the partition + gap space has 2*m + 1 total partitions. The lowest 
  # partition in the new space is 0.
  partitioned_nodes$partition <- 2*(partitioned_nodes$partition - 1) + 1
  
  # Select node to move.
  node <- sample(partitioned_nodes$node, size = 1)
  inode <- partitioned_nodes$node == node
  current_element <- partitioned_nodes[inode, 'partition']
  
  # Move node into another partition in the new space with uniform probability
  # for each partition element.
  move <- sample.int(2*m, size = 1)
  new_element <- (current_element + move) %% (2*m + 1)
  partitioned_nodes[inode, 'partition'] <- new_element
  
  # Relabel the elements back to the standard form.
  partitioned_nodes$partition <- match(
    partitioned_nodes$partition, 
    sort(unique(partitioned_nodes$partition))
  )
  partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)
  
  return(partitioned_nodes)
}

#' Calculate neighbourhood for node move.
#' 
#' @param partitioned_nodes A labelled partition.
#' 
#' @export
CalculateNodeMoveNeighbourhood <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  n <- nrow(partitioned_nodes)
  
  return(2*m*n)
}

