#' Default proposal constructor.
#' 
#' @param p Probability for each proposal in the order (split_join, node_move, 
#' swap_node, stay_still).
#' @param verbose Boolean flag to record proposal used.
#' 
#' @export
DefaultProposal <- function(p = c(0.34, 0.33, 0.33, 0.0), verbose = TRUE) {

  stopifnot(sum(p) == 1)
  
  function(partitioned_nodes) {
  
    alpha <- stats::runif(1)
    if (alpha < p[1]) {
      if (verbose) 
        proposal_info <- list(proposal_used = 'split_join')
      
      current_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
      
      proposed <- ProposePartitionSplitJoin(partitioned_nodes)
      partitioned_nodes <- proposed$partitioned_nodes
      rescore_nodes <- proposed$rescore_nodes
      
      new_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
    } else if (alpha <  sum(p[1:2])) {
      if (verbose)
        proposal_info <- list(proposal_used = 'node_move')
      
      current_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
      proposed <- ProposeNodeMove(partitioned_nodes)
      partitioned_nodes <- proposed$partitioned_nodes
      rescore_nodes <- proposed$rescore_nodes
      new_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
    } else if (alpha < sum(p[1:3])) {
      if (verbose)
        proposal_info <- list(proposal_used = 'swap_node')
      
      current_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
      proposed <- ProposeSwapNode(partitioned_nodes)
      partitioned_nodes <- proposed$partitioned_nodes
      rescore_nodes <- proposed$rescore_nodes
      new_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
    } else {
      if (verbose)
        proposal_info <- list(proposal_used = 'stay_still')
      
      current_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
      proposed <- ProposeStayStill(partitioned_nodes)
      partitioned_nodes <- proposed$partitioned_nodes
      rescore_nodes <- proposed$rescore_nodes
      new_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
    }
    
    return(list(proposal_info = proposal_info,
                state = partitioned_nodes,
                current_nbd = current_nbd,
                new_nbd = new_nbd,
                rescore_nodes = rescore_nodes))
  }
}
