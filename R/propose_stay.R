#' StayStill proposal.
#' 
#' @param verbose A flag to indicate that you are staying still.
#' 
#' @export
StayStill <- function(partitioned_nodes, verbose = FALSE) {
  
  current_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
  
  partitioned_nodes <- ProposeStayStill(partitioned_nodes, verbose)
  new_nbd <- CalculateStayStillNeighbourhood(partitioned_nodes)
  
  return(list(
    state = partitioned_nodes, 
    current_nbd = current_nbd, 
    new_nbd = new_nbd))
}

#' Propose that the partition stays still.
#' 
#' @examples 
#' ProposeStayStill(partitioned_nodes, verbose = TRUE)
#' 
#' @param partitioned_nodes A labelled partition.
#' @param verbose A flag to indicate that you are staying still.
#' 
#' @return A proposed labelled partition.
#' 
#' @export
ProposeStayStill <- function(partitioned_nodes, verbose = FALSE) {
  
  if (verbose) print('Staying still.')
  
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


