#' Convert DAG to partition.
#'
#' @description
#' This converts a DAG to it's partition by iteratively constructing sets of
#' outpoints. This is further explained in section 4.1 of Kuipers & Moffa (2017).
#'
#' @param dag A directed acyclic graph represented as an adjacency matrix, 
#' igraph, or bnlearn object.
#' 
#' @returns Labelled partition for the given adjacency matrix.
#' 
#' @examples
#' dag <- UniformlySampleDAG(LETTERS[1:3])
#' partitioned_nodes <- DAGtoPartition(dag)
#'
#' @references Kuipers, J., & Moffa, G. (2017). Partition MCMC for inference on 
#' acyclic digraphs. Journal of the American Statistical Association, 112(517), 
#' 282-299.
#'
#' @export
DAGtoPartition <- function(dag) UseMethod('DAGtoPartition')

#' @export
DAGtoPartition.default <- function(dag) {
  partition <- dag |>
    toMatrix() |>
    DAGtoPartition.matrix()
  
  return(partition)
}

#' @export
DAGtoPartition.matrix <- function(dag) {
  
  remaining_nodes <- colnames(dag)
  partitions <- c()
  nodes <- c()
  i <- 1
  while (length(remaining_nodes) > 1) {
    outpoint_bool <- colSums(dag[remaining_nodes, remaining_nodes]) == 0
    
    nodes <- c(nodes, names(which(outpoint_bool)))
    partitions <- c(partitions, rep(i, sum(outpoint_bool)))
    
    remaining_nodes <- names(which(!outpoint_bool))
    i <- i + 1
  }
  
  if (length(remaining_nodes) == 1) {
    nodes <- c(nodes, remaining_nodes)
    partitions <- c(partitions, i)
  }
  
  partitioned_nodes <- data.frame(partition = partitions, node = nodes)
  partitioned_nodes <- OrderPartitionedNodes(partitioned_nodes)
  
  return(partitioned_nodes)
}
