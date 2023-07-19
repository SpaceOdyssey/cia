#' Node move proposal.
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @export
NodeMove <- function(partitioned_nodes) {
  
  current_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
  
  proposed <- ProposeNodeMove(partitioned_nodes)
  partitioned_nodes <- proposed$partitioned_nodes
  rescore_nodes <- proposed$rescore_nodes
  
  new_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
  
  return(list(
    state = partitioned_nodes,
    current_nbd = current_nbd,
    new_nbd = new_nbd,
    rescore_nodes = rescore_nodes))
}

#' Propose individual node movement. 
#' 
#' This proposes that a single node selected uniformly can either:
#'   1) Move to any current partition.
#'   2) Move to any gap between or at the ends of the partitions.
#'   
#'  Any of these moves are possible and are selected uniformly with two 
#'  exceptions:
#'    1) The selected node cannot move into adjacent gaps if it originated from 
#'    a single node partition.
#'    2) The selected node cannot move to the immediately higher gap if it 
#'    originated from a two node partition.
#' 
#' @examples
#' dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' ProposeNodeMove(partitioned_nodes)
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @export
ProposeNodeMove <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  
  # Relabel current partitions into the new partition + gap space. Note that
  # the partition + gap space has 2*m + 1 total partitions. The lowest 
  # partition in the new space is 0.
  partitioned_nodes$partition <- 2*(partitioned_nodes$partition - 1) + 1
  
  # Select node to move.
  node <- sample(partitioned_nodes$node, size = 1)
  inode <- partitioned_nodes$node == node
  current_element <- partitioned_nodes$partition[inode]
  
  # Move the node into it's available 
  n_element <- sum(partitioned_nodes$partition == current_element)
  if (n_element == 1) {
    # Move node into any non-adjacent partition in the new space with uniform 
    # probability.
    move <- sample.int(2*m - 2, size = 1) + 1
  } else if (n_element == 2) {
    # Move node into any non-directly greater partition element in the new space
    # with uniform probability.
    move <- sample.int(2*m - 1, size = 1) + 1
  } else {
    # Move node into another partition in the new space with uniform probability
    # for each partition element.
    move <- sample.int(2*m, size = 1)
  }
  
  # Wrap move to deal with boundaries.
  new_element <- (current_element + move) %% (2*m + 1)
  
  rescore_nodes <- NodeMoveRescore(partitioned_nodes, node,
                                   current_element, new_element)
  
  # Assign and relabel the elements back to the standard form.
  partitioned_nodes$partition[inode] <- new_element
  partitioned_nodes$partition <- match(
    partitioned_nodes$partition, 
    sort(unique(partitioned_nodes$partition))
  )
  partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)

  return(list(partitioned_nodes = partitioned_nodes, 
              rescore_nodes = rescore_nodes))
}

#' Calculate neighbourhood for node move.
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @export
CalculateNodeMoveNeighbourhood <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  ordered_partition <- GetOrderedPartition(partitioned_nodes)
  
  n_1 <- sum(ordered_partition$frequency == 1)
  n_1_num_nbd <- (2*m - 2)*n_1
  
  n_2 <- 2*sum(ordered_partition$frequency == 2)
  n_2_num_nbd <- (2*m - 1)*n_2
  
  n_oth <- sum(ordered_partition$frequency[ordered_partition$frequency > 2])
  n_oth_num_nbd <- 2*m*n_oth
  
  return(n_1_num_nbd + n_2_num_nbd + n_oth_num_nbd)
}

#' Rescore nodes.
#' 
#' Find nodes to rescore. This works in the relabelled partition + gap space.
#' 
#' @noRd
NodeMoveRescore <- function(partitioned_nodes, node, old_element, new_element) {
  
  # Rescore nodes as a function of moved. Rescore based on:
  if (new_element > old_element) {
    start_rescore <- old_element + 1
    end_rescore <- new_element + ifelse(new_element %% 2 == 0, 1, 2)
    rescore_nodes <- c(node, GetPartitionNodes(partitioned_nodes, start_rescore:end_rescore))
  } else {
    start_rescore <- new_element + 1
    end_rescore <- old_element + 2
    rescore_nodes <- GetPartitionNodes(partitioned_nodes, start_rescore:end_rescore)
  }
  
  return(rescore_nodes)
}
