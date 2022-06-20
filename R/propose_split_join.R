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
#' This is the 'Basic Move' (i.e. algorithm 1) in Kuipers & Moffa (2015). There
#' is a caveat in that the split proposal for a partition with one element is
#' amiguous, as a split for such a partitin element results in a stay still 
#' proposal that has been removed.
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
    partition_j <- partitioned_nodes$partition[j]
    
    higher_partitions <- partitioned_nodes$partition > partition_j
    partitioned_nodes$partition[higher_partitions] <- partitioned_nodes$partition[higher_partitions] - 1
  } else {
    # Split a partition.
    
    # Find partition element to split. The split partition element needs to be
    # chosen proportional to the number of ways there is to perform the split.
    # Also record numnber of ways prior to the chosen partition.
    prior_i_star_num_nbd <- m - 1
    for (i_star in 1:m) {
      i_star_partitions <- partitioned_nodes[partitioned_nodes$partition <= i_star, ]
      i_star_num_nbd <- m - 1 + CalculateSplitCombinations(i_star_partitions)
      if (j <= i_star_num_nbd)
        break
      
      prior_i_star_num_nbd <- i_star_num_nbd
    }
    
    # Get split partition element information.
    ordered_partitions <- GetOrderedPartition(partitioned_nodes)
    k_i_star <- ordered_partitions[i_star, 'frequency']
    
    # Choose the number of nodes to select from the partition in proportion to
    # the number of ways available.
    choose_nodes <- 0
    for (c_star in 1:(k_i_star - 1)) {
      for (c in 1:c_star) {
        choose_nodes <- choose_nodes + choose(k_i_star, c)
      }
      
      if (j <= prior_i_star_num_nbd + choose_nodes)
        break
    }
    
    # Sample c_star nodes from partition element i_star.
    i_star_nodes <- partitioned_nodes[partitioned_nodes$partition == i_star, 'node']
    split_nodes <- sample(i_star_nodes, c_star)
    
    # Assign all partition elements higher than the chosen partition element to
    # one great.
    higher_elements <- partitioned_nodes$partition > i_star
    partitioned_nodes[higher_elements, 'partition'] <- partitioned_nodes[higher_elements, 'partition'] + 1
    
    # Move chosen nodes to a new partition element one greater than it's 
    # current partition element.
    move_nodes <- partitioned_nodes$node %in% split_nodes
    partitioned_nodes[move_nodes, 'partition'] <- i_star + 1
  }
  
  partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)
  
  return(partitioned_nodes)
}

#' Calculate neighbourhood for the split or join proposal.
#' 
#' The number of split combinations prescribed by KP15 is ambiguous when a 
#' partition element has only 1 node. A split for a partition element with 1 
#' node results in a proposal to stay still, as such I remove that proposal.
#' 
#' @export
CalculateSplitJoinNeighbourhood <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  split_combinations <- CalculateSplitCombinations(partitioned_nodes)
  
  return(m - 1 + split_combinations)
}

#' Calculate number of split combinations.
#' 
#' @export
CalculateSplitCombinations <- function(partitioned_nodes) {
  
  m <- GetNumberOfPartitions(partitioned_nodes)
  ordered_partition <- GetOrderedPartition(partitioned_nodes)
  
  split_combinations <- 0
  for (i in 1:m) {
    # Get number of combinations of splits for a given partition.
    k_i <- ordered_partition$frequency[i]
    if (k_i > 1) {
      for (c in 1:(k_i - 1)) {
        split_combinations <- split_combinations + choose(k_i, c)
      }
    }
  }
  
  return(split_combinations)
}
