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
  args$node <- node
  
  log_score <- as.numeric(do.call(BNLearnScoreSingleNode, args))
  
  return(log_score)
}

#' This is a rewritten version of bnlearn::score such that it calculates and 
#' returns the score for a single node. As this accesses non-exported functions
#' from bnlearn it could break if the underlying bnlearn code changes.
#' 
#' To limit the importation of non-exported functions I have removed several
#' checks that are performed by bnlearn::score.
#' 
#' @param x An object of class bn. 
#' @param data A data.frame of the data.
#' @param node A string representing the node to score.
#' @param type A string designating the score type as defined in bnlearn. 
#' Default is BIC.
#' @param ... Extra arguments to pass to the score function. See bnlearn for 
#' more details.
#' @param debug Whether to print out debugging information.  
#' 
#' @noRd
BNLearnScoreSingleNode <- function(x, data, node, type = NULL, ..., debug = FALSE) {
  
  # Check the score label.
  CheckScore <- utils::getFromNamespace('check.score', 'bnlearn')
  type = CheckScore(type, data)
    
  # Expand and sanitize score-specific arguments.
  CheckScoreArgs <- utils::getFromNamespace('check.score.args', 'bnlearn')
  extra.args = CheckScoreArgs(score = type, network = x,
                              data = data, extra.args = list(...), 
                              learning = FALSE)
  
  # compute the node contributions to the network score.
  PerNodeScore <- utils::getFromNamespace('per.node.score', 'bnlearn')
  local = PerNodeScore(network = x, data = data, score = type,
                       targets = node, extra.args = extra.args, 
                       debug = debug)
  
  return(local)
}

TemperedBNLearnScorer <- function(temperature) {
  
  stopifnot(temperature > 1.0)
  
  function(node, parents) {
    return(BNLearnScorer(node, parents)^(1.0/temperature))
  }
}
