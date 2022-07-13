## ---- include = FALSE--------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE--------------------------------------------------------------------------------------------------------
library(dagmc)
library(bnlearn)
library(dplyr)
library(ggplot2)

## ---- error = TRUE, fig.dim = c(6.0, 6.0)------------------------------------------------------------------------------------------
data <- bnlearn::gaussian.test
true_graph <- bnlearn::empty.graph(names(data))
arc_set <- matrix(c('A', 'F', 
                    'A', 'C', 
                    'B', 'C', 
                    'B', 'D',
                    # 'D', 'B',
                    'G', 'F',
                    'D', 'F',
                    'E', 'F'),
                  ncol = 2, byrow = TRUE,
                  dimnames = list(NULL, c('from', 'to')))
bnlearn::arcs(true_graph) <- arc_set
true_adj <- true_graph %>% 
  bnlearn::as.igraph() %>%
  igraph::as_adjacency_matrix() %>%
  as.matrix

## ---- fig.dim = c(6.0, 6.0)--------------------------------------------------------------------------------------------------------
plot(true_graph)

## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
scorer <- list(scorer = memo::memo(BNLearnScorer, memo::lru_cache(10000)), 
               parameters = list(data = data, type = 'bge'))

## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
set.seed(1)
n_results <- 2000
n_runs <- parallel::detectCores()

init_partitions <- list()
for (i in 1:n_runs) {
  init_dag <- UniformlySampleDAG(names(data))
  init_partitions[[i]] <- GetPartitionedNodesFromAdjacencyMatrix(init_dag)
}

system.time(
  chains <- SampleChains(n_results, init_partitions, 
                         transition = PartitionMCMC(),
                         scorer = scorer,
                         n_parallel_chains = n_runs)
)

## ---- error = TRUE, fig.dim = c(6.0, 4.0)------------------------------------------------------------------------------------------
PlotScoreTrace <- function(chains, burnin = 1, nrow = NULL, ncol = 1, ...) {
  
  colors <- c('black', 'green', 'red', 'blue', 'brown', 'darkviolet', 
              'darkgoldenrod', 'deeppink')
  
  n_within <- length(chains[[1]]$log_score)
  plot(chains[[1]]$log_score[burnin:n_within], col = colors[1], ...)
  
  for (i in 2:length(chains)) {
    n_within <- length(chains[[i]]$log_score)
    lines(chains[[i]]$log_score[burnin:n_within], 
          col = colors[i %% length(colors)], ...)
  }
}

PlotScoreTrace(chains, ylab = 'log(partition score)', type = 'l')

## ---- error = TRUE, fig.dim = c(6.0, 4.0)------------------------------------------------------------------------------------------
n_burnin <- min(500, as.integer(0.5*n_results))
PlotScoreTrace(chains, burnin = n_burnin, ylab = 'log(partition score)', type = 'l', ylim = c(-53275, -53255))

## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
set.seed(1)
dags <- list()
score_dags <- list()
for (i in 1:n_runs) {
  eq_states <- chains[[i]]$state[(1 + n_burnin):n_results]
  dags[[i]] <- lapply(eq_states, SampleDAGFromLabelledPartition, scorer = scorer)
  score_dags[[i]] <- unlist(lapply(dags[[i]], ScoreDAG, scorer = scorer))
}

## ---- error = TRUE, fig.dim = c(6.0, 4.0)------------------------------------------------------------------------------------------
true_score <- ScoreDAG(true_adj, scorer)
n_target_saves <- n_results - n_burnin
colors <- c('black', 'green', 'red', 'blue', 'brown', 'darkviolet', 'darkgoldenrod', 'deeppink')
plot(1:n_target_saves, score_dags[[1]], xlab = 'saves', ylab = 'log(DAG score)', type = 'l')
for (i in 2:n_runs) {
  lines(score_dags[[i]], xlab = 'saves', ylab = 'log(partition score)', type = 'l', col = colors[i - 1])
}

## ---- error = TRUE, fig.dim = c(6.0, 6.0)------------------------------------------------------------------------------------------
flat_score_dags <- unlist(score_dags)
flat_dags <- unlist(dags, recursive = FALSE)
                    
imap <- which(flat_score_dags == max(flat_score_dags))[1]
map_mat <- flat_dags[[imap]]

map_dag <- bnlearn::empty.graph(colnames(data))
amat(map_dag) <- map_mat

plot(map_dag)

## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
chain_info <- list()
for (i in 1:n_runs) {
  chain_info[[i]] <- dplyr::bind_cols(
    proposal_used = unlist(chains[[i]]$proposal_info[n_burnin:(n_results - 1)], use.names = FALSE),
    accept = unlist(chains[[i]]$mcmc_info[n_burnin:(n_results - 1)], use.names = FALSE)
    )
}
chain_info <- dplyr::bind_rows(chain_info)

prop_accept <- chain_info %>% 
  dplyr::group_by(proposal_used) %>% 
  dplyr::summarise(accept = mean(accept))

knitr::kable(prop_accept)

