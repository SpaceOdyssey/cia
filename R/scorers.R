# Scorers.
#
# Scorers are lists with elements:
#   scorer: Callable that scores a (node, parents) combination. It must be of 
#     the form fun(node, parents, ...) .
#   parameters: A list of parameter (name, value) pairs to pass to the scorer
#     callable.

#' Scorer constructor.
#' 
#' @param scorer A scorer function that takes (node, parents) as parameters. 
#' Default is BNLearnScorer.
#' @param ... Parameters to pass to scorer. 
#' @param max_parents The maximum number of allowed parents. Default is 
#' infinite.
#' @param blacklist A boolean matrix of (parent, child) pairs where TRUE 
#' represents edges that cannot be in the DAG. Default is NULL which 
#' represents no blacklisting.
#' @param whitelist A boolean matrix of (parent, child) pairs where TRUE 
#' represents edges that must be in the DAG. Default is NULL which represents 
#' no whitelisting.
#' @param cache A boolean to indicate whether to build the cache. The 
#' cache only works for problems where the scorer only varies as a function of 
#' (node, parents). Default is FALSE.
#' @param nthreads Number of threads used to construct cache.
#' 
#' @examples
#' scorer <- CreateScorer(data = bnlearn::asia)
#' 
#' @export
CreateScorer <- function(scorer = BNLearnScorer, ..., max_parents = Inf, 
                         blacklist = NULL, whitelist = NULL, cache = FALSE,
                         nthreads = 1) {
  
  scorer <- list(scorer = scorer,
                 parameters = list(...),
                 max_parents = max_parents,
                 blacklist = blacklist,
                 whitelist = whitelist)
  
  if (cache)
    scorer$scorer <- CachedScorer(scorer, max_size = NULL, nthreads = nthreads)
  
  return(scorer)
}

#' BNLearnScorer
#' 
#' @description 
#' A thin wrapper on the bnlearn::score function.
#' 
#' @param node Name of node to score.
#' @param parents The parents of node.
#' @param ... The ellipsis is used to pass other parameters to the scorer.
#' 
#' @examples
#' data <- bnlearn::learning.test
#' BNLearnScorer('A', c('B', 'C'), data = data)
#' BNLearnScorer('A', c(), data = data)
#' BNLearnScorer('A', vector(), data = data)
#' BNLearnScorer('A', NULL, data = data)
#' BNLearnScorer('A', c('B', 'C'), data = data, type = "bde", iss = 100)
#' BNLearnScorer('A', c('B', 'C'), data = data, type = "bde", iss = 1)
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
  CheckData <- utils::getFromNamespace('check.data', 'bnlearn')
  data <- CheckData(data)
  
  CheckScore <- utils::getFromNamespace('check.score', 'bnlearn')
  type <- CheckScore(type, data)
    
  # Expand and sanitize score-specific arguments.
  CheckScoreArgs <- utils::getFromNamespace('check.score.args', 'bnlearn')
  extra.args <- CheckScoreArgs(score = type, network = x,
                               data = data, extra.args = list(...), 
                               learning = FALSE)
  
  # Compute the node score.
  PerNodeScore <- utils::getFromNamespace('per.node.score', 'bnlearn')
  local <- PerNodeScore(network = x, data = data, score = type,
                        targets = node, extra.args = extra.args, 
                        debug = debug)
  
  return(local)
}
