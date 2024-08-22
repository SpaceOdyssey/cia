#' Plot cumulative mean trace plot.
#'
#' @param x A posterior predictive sample object.
#' @param scales Whether the scales should the fixed ('fixed', the default), 
#' free ('free') or free in one dimension ('free_x', 'free_y')? 
#' @param ncol Number of columns.
#' @param nrow Number of rows.
#' @param dir Direction to fill facets. Either 'h' for horizontal or 'v' for
#' vertical.
#' 
#' @export
PlotCumulativeMeanTrace <- function(x, ncol = NULL, nrow = NULL, scales = 'fixed', dir = 'v') UseMethod('PlotCumulativeMeanTrace')

#' @export
PlotCumulativeMeanTrace.cia_post_chains <- function(x, ncol = NULL, nrow = NULL, scales = 'fixed', dir = 'v') {
  
  cummean <- x |>
    lapply(function(x) {
      x |>
        apply(2, function(y) {
          cummean <- cumsum(y) / seq(1, length(y))
        })
    }) |> 
    lapply(
      function(x) x |>
        as.data.frame() |>
        dplyr::mutate(iteration = dplyr::row_number())
      ) |>
    dplyr::bind_rows(.id = 'chain') |>
    tidyr::pivot_longer(-c(.data$chain, .data$iteration))
  
  # Between chains.
  g1 <- cummean |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data$iteration, 
                   y = .data$value, 
                   color = .data$chain)
      ) +
    ggplot2::facet_wrap(~name, 
                        scales = 'free_y',
                        nrow = nrow,
                        ncol = ncol,
                        dir = dir) +
    ggplot2::geom_line() +
    ggplot2::ylab('Cumulative Mean') +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = 'none')
  
  return(g1)
}
