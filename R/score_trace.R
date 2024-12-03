#' Plot the score trace
#' 
#' @param chains MCMC chains.
#' @param attribute Name of attribute to plot. Default is "log_score".
#' @param n_burnin Number of steps to remove as burnin.
#' @param same_plot Whether to plot on the same figure or on multiple figures.
#' @param col A string representing a color for a single chain or a vector of 
#' strings to cycle through for multiple chains.
#' 
#' @returns Depending on the argument 'same_plot', either:
#' - A single 'ggplot' object combining all chains into one plot 
#' - A list of 'ggplot' objects, each corresponding to a separate chain 
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
#' PlotScoreTrace(results)
#' 
#' # Plot DAG score trace.
#' dag_chains <- PartitiontoDAG(results, scorer)
#' PlotScoreTrace(dag_chains)
#' 
#' @export
PlotScoreTrace <- function(chains, attribute = 'log_score', n_burnin = 0, 
                           same_plot = TRUE, col = NULL) UseMethod('PlotScoreTrace')

#' @export
PlotScoreTrace.cia_chains <- function(chains, attribute = 'log_score', n_burnin = 0, 
                                      same_plot = TRUE, col = NULL) {
  plot_list <- list()
  if (is.null(col))
    col <- c('black', 'green', 'red', 'blue', 'brown', 'darkviolet', 
             'darkgoldenrod', 'deeppink')
  n_within <- length(chains[[1]][[attribute]])
  chain_log_score <- data.frame(
    log_score = chains[[1]][[attribute]][(1 + n_burnin):n_within],
    iteration = 1:length(chains[[1]][[attribute]]))
  
  plots <- ggplot2::ggplot(chain_log_score, ggplot2::aes(x = iteration, y = log_score)) +
    ggplot2::geom_line(size = 0.4, colour = col[[1]]) + 
    ggplot2::labs(x = "Iteration", y = attribute) + 
    ggplot2::theme_classic() 
  
  n_chains <- length(chains)
  if(n_chains > 1) {
    for(i in 2:length(chains)) {
      chain_log_score <- data.frame(
        log_score = chains[[i]][[attribute]],
        iteration = 1:length(chains[[i]][[attribute]]))
      if (same_plot){
        plots <- plots + ggplot2::geom_line(data = chain_log_score, ggplot2::aes(x = iteration, y = log_score), size = 0.4, colour = col[[i]])
        print(plots)
      } else {
        
        plot_list[[1]] <- plots
        print(plots)
        plot_list[[i]] <- ggplot2::ggplot(chain_log_score, ggplot2::aes(x = iteration, y = log_score)) +
          ggplot2::geom_line(size = 0.4, colour = col[[i]]) + 
          ggplot2::labs(x = "Iteration", y = "log score") + 
          ggplot2::theme_classic()
        print(plot_list[[i]])
      }
      
    } 
  }
  if(same_plot){
    return(plots)
  } else {
    return(plot_list)
  }
}

#' @export
PlotScoreTrace.cia_chain <- function(chains, attribute = 'log_score', n_burnin = 0, 
                                     same_plot = TRUE, col = NULL) {
  
  if (is.null(col))
    col <- 'black'
  
  n_within <- length(chains[[attribute]])
  chain_log_score <- data.frame(
    log_score = chains[[attribute]][(1 + n_burnin):n_within],
    iteration = 1:length(chains[[attribute]]))
  plot <- ggplot2::ggplot(chain_log_score, ggplot2::aes(x = iteration, y = log_score)) +
    ggplot2::geom_line(size = 0.4, colour = col[[1]]) + 
    ggplot2::labs(x = "Iteration", y = attribute) + 
    ggplot2::theme_classic() 
  return(plot)
}