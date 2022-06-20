#' Default proposal
#' 
#' @export
DefaultProposal <- function(p_split_join = 0.4, p_swap_node = 0.30, 
                            p_node_move = 0.29, verbose = FALSE) {

  stopifnot(sum(p_split_join, p_swap_node, p_node_move) <= 1)
  
  cdf_swap_node <- p_split_join + p_swap_node
  cdf_node_move <- cdf_swap_node + p_node_move
  
  function(partitioned_nodes) {
  
    alpha <- stats::runif(1)
    if (alpha < p_split_join) {
      proposal_used <- 'split_join'
      current_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposePartitionSplitJoin(partitioned_nodes, verbose = verbose)
      new_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
    } else if (alpha < cdf_swap_node) {
      proposal_used <- 'swap_node'
      current_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposeSwapNode(partitioned_nodes, verbose = verbose)
      new_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)    
    } else if (alpha < cdf_node_move) {
      proposal_used <- 'node_move'
      current_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposeNodeMove(partitioned_nodes, verbose = verbose)
      new_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
    } else {
      proposal_used <- 'stay_still'
      current_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposeStayStill(partitioned_nodes, verbose = verbose)
      new_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
    }
    
    return(list(
      proposal_used = proposal_used,
      state = partitioned_nodes, 
      current_nbd = current_nbd, 
      new_nbd = new_nbd)
      )
  }
}
