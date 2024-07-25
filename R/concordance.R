#' Concordance plot as outl.
#' 
#' This is a thin wrapper to pass dagmc objects to the BiDAG concordance
#' plot code.
#' 
#' @param x A list of adjacency matrices representing edge probabilities, a 
#' chains object, or a collections object with states as DAGs.
#' @param ... Additional parameter to send to the appropriate method. This includes
#' 'highlight' (defauled to 0.3) which sets the cutoff difference that is used to
#' highlight the points, and the probability edge estimation 'method' for a 
#' dagmc_collections object.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' dag <- UniformlySampleDAG(colnames(data))
#' partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
#' scorer <- CreateScorer(scorer = BNLearnScorer, data = data)
#' 
#' results <- SampleChains(300, partitioned_nodes, PartitionMCMC(), scorer, n_parallel_chains = 4)
#' dags <- PartitiontoDAG(results, scorer)
#' PlotConcordance(dags)
#' 
#' # OR
#' p_edge <- CalculateEdgeProbabilities(dags)
#' PlotConcordance(p_edge)
#' 
#' @export
PlotConcordance <- function(x, ...) UseMethod('PlotConcordance')

#' @export
PlotConcordance.default <- function(x, highlight = 0.3, ...) {
  
  n_runs <- length(x)
  
  if (n_runs == 2) {
    plt <- PlotConcordanceSingle(x[[1]], x[[2]], highlight)
  } else {
    g <- list()
    k <- 1
    for (i in 1:n_runs) {
      for (j in 1:n_runs) {
        if (i == j) { 
          text <- paste('Run', (k - 1) %/% 4 + 1)
          g[[k]] <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0, y = 0, label = text) +
            ggplot2::theme_void()
        } else {
          g[[k]] <- PlotConcordanceSingle(x[[i]], x[[j]], highlight)
        }
        k <- k + 1
      }
    }
    plt <- patchwork::wrap_plots(g, nrow = n_runs, ncol = n_runs)
  }
  
  return(plt)
}

#' @export
PlotConcordance.list <- function(x, highlight = 0.3, ...) {

  g <- PlotConcordance.default(x, highlight = highlight)
  
  return(g)
}

#' @export
PlotConcordance.dagmc_chains <- function(x, highlight = 0.3, ...) {
  
  p_edge <- CalculateEdgeProbabilities(x)
  g <- PlotConcordance(p_edge, highlight = highlight)
  
  return(g)
}

#' @export
PlotConcordance.dagmc_collections <- function(x, highlight = 0.3, method = 'sampled', ...) {

  p_edge <- CalculateEdgeProbabilities(x, highlight = highlight, method = method)
  g <- PlotConcordance(p_edge)
  
  return(g)

}

#' @noRd
PlotConcordanceSingle <- function(x_state, y_state, highlight = 0.3) {
  
  diag(x_state) <- NA
  x_k <- as.vector(x_state)
  x_k <- x_k[!is.na(x_k)]
  
  diag(y_state) <- NA
  y_k <- as.vector(y_state)
  y_k <- y_k[!is.na(y_k)]
  
  h_k <- abs(x_k - y_k) > highlight
  
  x_k_g <- as.vector(x_k[!h_k])
  y_k_g <- as.vector(y_k[!h_k])
  
  x_k_r <- as.vector(x_k[h_k])
  y_k_r <- as.vector(y_k[h_k])
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_abline(slope = 1.0) 
  
  if (length(x_k_g) > 0)
    g <- g + 
    ggplot2::geom_point(ggplot2::aes(x = x_k_g, y = y_k_g),
                        shape = 21,
                        fill = NA,
                        colour = 'grey')
  
  if (length(x_k_r) > 0)
    g <- g +             
    ggplot2::geom_point(ggplot2::aes(x = x_k_r, y = y_k_r), 
                        shape = 21,
                        fill = NA,
                        colour = 'red')
  
  g <- g +
    ggplot2::scale_x_continuous(name = ggplot2::element_blank(),
                                breaks = c(0.0, 0.5, 1.0)) +
    ggplot2::scale_y_continuous(name = ggplot2::element_blank(),
                                breaks = c(0.0, 0.5, 1.0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = 'none', aspect.ratio = 1.0)
  
  return(g)
}
