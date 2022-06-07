#' Partition split or join constructor.
#' 
#' @export
PartitionSplitJoin <- function(partitioned_nodes, verbose = FALSE) {
  
  current_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
  
  partitioned_nodes <- ProposePartitionSplitJoin(partitioned_nodes, verbose)
  new_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
  
  return(list(
    state = partitioned_nodes, 
    current_nbd = current_nbd, 
    new_nbd = new_nbd))
}

#' Propose a split or join of two partitions. 
#' 
#' @description
#' This is the 'Basic Move' (i.e. algorithm 1) in Kuipers & Moffa (2015).
#' 
#' @examples 
#' ProposePartitionSplitJoin(partitioned_nodes, verbose = TRUE)
#' 
#' @param partitioned_nodes A labelled partition.
#' @param verbose A flag to indicate whether to inform the user that the move 
#' is a split or join.
#' 
#' @return A proposed labelled partition.
#' 
#' @export
ProposePartitionSplitJoin <- function(partitioned_nodes, verbose = FALSE) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  num_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
  j <- sample.int(num_nbd, size = 1)
  if (j < m) {
    # Join two partitions.
    if (verbose) print('Joining two partition elements.')
    
    partition_j <- partitioned_nodes$partition[j]
    
    higher_partitions <- partitioned_nodes$partition > partition_j
    partitioned_nodes$partition[higher_partitions] <- partitioned_nodes$partition[higher_partitions] - 1
  } else {
    # Split a partition.
    if (verbose) print('Splitting partition element.')
    
    # Find partition element to source nodes. To do this we find minimum i* for 
    # given condition.
    for (i_star in 1:m) {
      i_star_partitions <- partitioned_nodes[partitioned_nodes$partition <= i_star, ]
      i_star_num_nbd <- CalculateSplitJoinNeighbourhood(i_star_partitions)
      if (j <= i_star_num_nbd)
        break
    }
    
    # Get partition element information.
    ordered_partitions <- GetOrderedPartition(partitioned_nodes)
    k_i_star <- ordered_partitions[i_star, 'frequency']
    
    # Only move if there are at least two nodes in the partition element.
    if (k_i_star > 1) {
      
      # Find minimum c* for given condition.
      for (c_star in 1:k_i_star) {
        for (c in 1:c_star) {
          choose_nodes <- choose(k_i_star, c)
        }
        
        if (j <= i_star_num_nbd + choose_nodes)
          break
      }
      
      # Sample c_star nodes from partition element i_star.
      i_star_nodes <- partitioned_nodes[partitioned_nodes$partition == i_star, 'node']
      split_nodes <- sample(i_star_nodes, c_star)
      
      # Move them to the left. NOTE: I have my partition elements ordered in the 
      # opposite direction to kp15. I should probably change that for consistency.
      if (i_star < m) {
        higher_elements <- partitioned_nodes$partition > i_star
        partitioned_nodes[higher_elements, 'partition'] <- partitioned_nodes[higher_elements, 'partition'] + 1
      }
      
      move_nodes <- partitioned_nodes$node %in% split_nodes
      partitioned_nodes[move_nodes, 'partition'] <- i_star + 1
    }
  }
  
  partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)
  
  return(partitioned_nodes)
}

#' Calculate neighbourhood for the split or join proposal.
#' 
#' @export
CalculateSplitJoinNeighbourhood <- function(partitioned_nodes) {
  
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
