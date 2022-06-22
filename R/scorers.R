# Scorers.
#
# Scorers are lists with elements:
#   scorer: Callable that scores a (node, parents) combination. It must be of 
#     the form fun(node, parents, ...) .
#   parameters: A list of parameter (name, value) pairs to pass to the scorer
#     callable.

#' BNLearnScorer
#' 
#' @param node A child node of parents.
#' @param parents The parent nodes of node.
#' @param ... The ellipsis is used to pass other parameters to the scorer.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' BNLearnScorer('A', c('B', 'C'), data = data)
#' BNLearnScorer('A', c(), data = data)
#' BNLearnScorer('A', vector(), data = data)
#' BNLearnScorer('A', NULL, data = data)
#' BNLearnScorer('A', c('B', 'C'), data = data, type = 'bde', iss = 100)
#' BNLearnScorer('A', c('B', 'C'), data = data, type = 'bde', iss = 1)
#'
#' @export
BNLearnScorer <- function(node, parents, ...) {
  args <- list(...)
  args$data <- args$data[, c(node, parents), drop = FALSE]
  
  if (is.null(parents))
    parents <- c()
  
  dag <- bnlearn::empty.graph(c(node, parents))
  arc_set <- matrix(
    c(parents, rep(node, length(parents))),
    ncol = 2, dimnames = list(NULL, c('from', 'to'))
  )
  bnlearn::arcs(dag) <- arc_set
  args$x <- dag
  args$by.node = TRUE
  
  log_score <- do.call(bnlearn::score, args)[[node]]
  
  return(log_score)
}

#' Create scorer object. Not implemented yet.
#' 
#' @examples
#' scorer_1 <- list(
#'   scorer = BNLearnScorer,
#'   parameters = list(data = data, type = 'bde', iss = 100)
#'   )
#'
#' scorer_2 <- list(
#'  scorer = BNLearnScorer,
#'  parameters = list(data = data)
#' )
#' 
#' @noRd
create_scorer <- function(FUN, ...) {
  
  scorer <- list(
    scorer = FUN,
    parameters = list(...)
  )
  
  return(scorer)
}
