#' Sample DAGs from labelled partitions.
#' 
#' @param partitions A dagms_chains, cia_chain, or matrix.
#' @param scorer A scorer object.
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
PartitiontoDAG.matrix <- function(partitions, scorer) {
  dag <- SampleDAGFromLabelledPartition(partitions, scorer)
  
  return(dag)
}
