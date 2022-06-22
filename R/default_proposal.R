#' Default proposal constructor.
#' 
#' @param p Probability for each proposal in the order (split_join, node_move, 
#' swap_node, stay_still).
#' @param verbose Boolean flag to record proposal used.
#' 
#' @export
DefaultProposal <- function(p = c(0.33, 0.33, 0.33, 0.01), verbose = TRUE) {

  stopifnot(sum(p) == 1)
  
  function(partitioned_nodes) {
  
    alpha <- stats::runif(1)
    if (alpha < p[1]) {
      if (verbose) 
        proposal_info <- list(proposal_used = 'split_join')
      
      current_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposePartitionSplitJoin(partitioned_nodes)
      new_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
    } else if (alpha <  sum(p[1:2])) {
      if (verbose)
        proposal_info <- list(proposal_used = 'node_move')
      
      current_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposeNodeMove(partitioned_nodes)
      new_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
    } else if (alpha < sum(p[1:3])) {
      if (verbose)
        proposal_info <- list(proposal_used = 'swap_node')
      
      current_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposeSwapNode(partitioned_nodes)
      new_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
    } else {
      if (verbose)
        proposal_info <- list(proposal_used = 'stay_still')
      
      current_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
      partitioned_nodes <- ProposeStayStill(partitioned_nodes)
      new_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
    }
    
    return(list(
      proposal_info = proposal_info,
      state = partitioned_nodes, 
      current_nbd = current_nbd, 
      new_nbd = new_nbd)
      )
  }
}
