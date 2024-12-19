#' Sample DAG from partition
#' 
#' Samples a DAG in accordance with it's posterior probability conditional
#' on it being consistent with a partition.
#' 
#' @param partitions A cia_chain(s) object or data.frame representing the 
#' partition.
#' @param scorer A scorer object.
#' 
#' @returns A cia_chain(s) object or adjacency matrix. For a cia_chain(s) object
#' each state will be an adjacency matrix.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partition <- DAGtoPartition(dag)
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data
#'   )
#' 
#' # Used to sample from a single partition.  
#' PartitiontoDAG(partition, scorer)
#' 
#' # Used to convert a chain of partitions to DAGs.
#' init_state <- InitPartition(colnames(data), scorer)
#' results <- SampleChains(3, init_state, PartitionMCMC(), scorer)
#' PartitiontoDAG(results, scorer)
#' 
#' @export
PartitiontoDAG <- function(partitions, scorer) UseMethod('PartitiontoDAG')

#' @export
PartitiontoDAG.cia_chains <- function(partitions, scorer) {
  
  n_chains <- length(partitions)
  
  cl <- parallel::makeCluster(n_chains)
  doParallel::registerDoParallel(cl)
  i <- NULL
  chains <- foreach::foreach(i = 1:n_chains) %dopar% {
    PartitiontoDAG(partitions[[i]], scorer)
  }
  parallel::stopCluster(cl)
  
  chains <- new_cia_chains(chains)
  
  return(chains)
}

#' @export
PartitiontoDAG.cia_chain <- function(partitions, scorer) {
  
  n_results <- length(partitions$state)
  
  chain <- list()
  chain$state <- list()
  chain$log_score <- vector('numeric', length = n_results)
  
  for (i in 1:n_results) {
    dag <- SampleDAGFromLabelledPartition(partitions$state[[i]], scorer)
    chain$state[[i]] <- dag$state
    chain$log_score[i] <- dag$log_score
  }
  
  chain <- new_cia_chain(chain)
  
  return(chain)
}

#' @export
PartitiontoDAG.data.frame <- function(partitions, scorer) {
  dag <- SampleDAGFromLabelledPartition(partitions, scorer)
  
  return(dag)
}
