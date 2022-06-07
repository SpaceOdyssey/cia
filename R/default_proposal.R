#' Default proposal
#' 
#' @export
DefaultProposal <- function(partitioned_nodes, verbose = FALSE) {
  
  alpha <- runif(1)
  if (alpha < 0.4) {
    current_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
    partitioned_nodes <- ProposePartitionSplitJoin(partitioned_nodes, verbose = verbose)
    new_nbd <- CalculateSplitJoinNeighbourhood(partitioned_nodes)
  } else if (alpha < 0.695) {
    current_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)
    partitioned_nodes <- ProposeSwapNode(partitioned_nodes, verbose = verbose)
    new_nbd <- CalculateSwapNodeNeighbourhood(partitioned_nodes)    
  } else if (alpha < 0.99) {
    current_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
    partitioned_nodes <- ProposeNodeMove(partitioned_nodes, verbose = verbose)
    new_nbd <- CalculateNodeMoveNeighbourhood(partitioned_nodes)
  } else {
    current_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
    partitioned_nodes <- ProposeStayStill(partitioned_nodes, verbose = verbose)
    new_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
  }
  
  return(list(
    state = partitioned_nodes, 
    current_nbd = current_nbd, 
    new_nbd = new_nbd)
    )
}
