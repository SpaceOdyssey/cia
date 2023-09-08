#' Sample a DAG from a labelled partition.
#' 
#' @description
#' Sample a single DAG with  from the set of DAGs that is consistent with a labelled
#' partition. That is, given a set of DAGs \eqn{\mathcal{G}} that is consistent
#' with a labelled partition \eqn{\Lambda} then it will sample a given DAG 
#' \eqn{G} where \eqn{G \in \mathcal{G}} with probability 
#' \eqn{p(G|D, \Lambda) = p(G)p(D | G) / \sum_{G \in \mathcal{G}} p(G)p(D | G)}.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' 
#' scorer <- CreateScorer(data = data)
#' 
#' SampleDAGFromLabelledPartition(partitioned_nodes, scorer)
#' 
#' @param partitioned_nodes Labelled partition.
#' @param scorer Scorer object.
#' 
#' @returns A list with elements:
#'  \itemize{
#'    \item state Adjacency matrix.
#'    \item log_score Score of the sampled DAG.
#'  }
#' 
#' @export
SampleDAGFromLabelledPartition <- function(partitioned_nodes, scorer) {
  
  nodes <- sort(partitioned_nodes$node)
  dag <- matrix(
    0L,
    nrow = length(nodes), 
    ncol = length(nodes), 
    dimnames = list(nodes, nodes)
  )
  
  log_score <- 0.0
  for (node in nodes) {
    score_table <- ScoreTableNode(partitioned_nodes, node, scorer)
    
    # Normalise score table.
    log_z <- LogSumExp(score_table$log_scores)
    norm_score_table <- score_table$log_scores - log_z
    
    # Create unordered cumulative distribution function.
    n_scores <- length(score_table$log_scores)
    cdf_log_p <- sapply(1:n_scores, function(x) LogSumExp(norm_score_table[1:x]))
    
    # Select parents using the unordered cumulative distribution.
    log_alpha <- log(stats::runif(1))
    i_parents <- min(which(log_alpha < cdf_log_p))
    parents <- score_table$parent_combinations[[i_parents]]

    # Add (node, parents) to DAG.
    dag[parents, node] <- 1L
    log_score <- log_score + score_table$log_scores[i_parents]
  }
  
  return(list(state = dag, log_score = log_score))
}
