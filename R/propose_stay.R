#' StayStill proposal.
#' 
#' @param partitioned_nodes Labelled partition.
#' 
#' @export
StayStill <- function(partitioned_nodes) {
  
  current_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
  
  partitioned_nodes <- ProposeStayStill(partitioned_nodes)
  new_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
  
  return(list(
    state = partitioned_nodes, 
    current_nbd = current_nbd, 
    new_nbd = new_nbd))
}

#' Propose that the partition stays still.
#' 
#' @examples
#' dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' ProposeStayStill(partitioned_nodes)
#' 
#' @param partitioned_nodes A labelled partition.
#' 
#' @return A proposed labelled partition.
#' 
#' @export
ProposeStayStill <- function(partitioned_nodes) {
  return(partitioned_nodes)
}

#' Calculate neighbourhood for staying still.
#' 
#' @param partitioned_nodes A labelled partition.
#' 
#' @export
CalculateStayStillNeighbourhood <- function(partitioned_nodes) {
  return(1)
}


