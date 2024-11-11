#' Plot the score trace
#' 
#' @param chains MCMC chains.
#' @param attribute Name of attribute to plot. Default is "log_score".
#' @param n_burnin Number of steps to remove as burnin.
#' @param same_plot Whether to plot on the same figure or on multiple figures.
#' @param col A string representing a color for a single chain or a vector of 
#' strings to cycle through for multiple chains.
#' @param ... Extra parameters to pass to the plot and graphics::line functions.
#' 
#' @returns No return value. Called to produce a base R trace plot.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' 
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- DAGtoPartition(dag)
#' 
#' scorer <- CreateScorer(
#'   scorer = BNLearnScorer, 
#'   data = data
#'   )
#' 
#' results <- SampleChains(10, partitioned_nodes, PartitionMCMC(), scorer)
#' 
#' # Plot partition score trace.
#' PlotScoreTrace(results, type = 'l')
#' 
#' # Plot DAG score trace.
#' dag_chains <- PartitiontoDAG(results, scorer)
#' PlotScoreTrace(dag_chains, type = 'l')
#' 
#' @export
PlotScoreTrace <- function(chains, attribute = 'log_score', n_burnin = 0, 
                           same_plot = TRUE, col = NULL, ...) UseMethod('PlotScoreTrace')

#' @export
PlotScoreTrace.cia_chains <- function(chains, attribute = 'log_score', n_burnin = 0, 
                                        same_plot = TRUE, col = NULL, ...) {
  
  if (is.null(col))
    col <- c('black', 'green', 'red', 'blue', 'brown', 'darkviolet', 
             'darkgoldenrod', 'deeppink')
  
  n_within <- length(chains[[1]][[attribute]])
  plot(chains[[1]][[attribute]][(1 + n_burnin):n_within], col = col[1], ...)
  
  n_chains <- length(chains)
  if (n_chains > 1) {
    for (i in 2:length(chains)) {
      n_within <- length(chains[[i]][[attribute]])
      if (same_plot) {
        graphics::lines(chains[[i]][[attribute]][(1 + n_burnin):n_within], 
                        col = col[i %% length(col)], ...)
      } else {
        plot(chains[[i]][[attribute]][(1 + n_burnin):n_within], 
             col = col[i %% length(col)], ...)
      }
    }
  }
}

#' @export
PlotScoreTrace.cia_chain <- function(chains, attribute = 'log_score', n_burnin = 0, 
                                       same_plot = TRUE, col = NULL, ...) {
  
  if (is.null(col))
    col <- 'black'
  
  n_within <- length(chains[[attribute]])
  plot(chains[[attribute]][(1 + n_burnin):n_within], col = col, ...)
}
