#' This builds the score cache. It can be used for problems where the score only
#' changes as a function of (node, parents). 
#' 
#' @param scorer A scorer.
#' @param max_size Not implemented. Maximum number of scores to store in the 
#' cache. If the total number of combinations is greater than this number then 
#' the cache follows a least recently used replacement policy.
#' 
#' @examples
#' scorer <- CreateScorer(data = bnlearn::learning.test)
#' cached_scorer <- CachedScorer(scorer)
#' cached_scorer('A', c('B', 'C'))
#' 
#' @export
CachedScorer <- function(scorer, max_size = NULL) {
  
  cache <- BuildCache(scorer, max_size = max_size)
  function(node, parents, ...) {
    nodes <- names(scorer$parameters$data)
    parents_key <- GetParentsKey(parents, nodes)
    log_score <- cache[[node]][[parents_key]]
    
    return(log_score)
  }
}

#' Build cached scores.
#' 
#' @param scorer Scorer object.
#' @param max_size Not implemented.
#' 
#' @noRd
BuildCache <- function(scorer, max_size = NULL) {
  
  nodes <- names(scorer$parameters$data)
  cache <- list()
  for (node in nodes) {
    # Remove disallowed parents.
    blacklisted_nodes <- GetRestrictedParents(node, scorer$blacklist)
    allowed_nodes <- nodes %>%
      setdiff(node) %>%
      setdiff(blacklisted_nodes)
    n_allowed <- length(allowed_nodes)
    
    if (n_allowed == 0)
      next
    
    # Calculate score for allowed parent combinations.
    scorer$parameters$node <- node
    node_cache <- list()
    
    # Add orphaned child.
    parents_key <- GetParentsKey(c(), nodes)
    scorer$parameters$parents <- vector()
    node_cache[[parents_key]] <- do.call(scorer$scorer, scorer$parameters)
    
    # Add all other possibilities.
    # Get all possible combinations of allowed parents subject to max_parents.
    max_parents <- min(n_allowed, scorer$max_parents)
    parent_coms <- (1:max_parents) %>%
      lapply(function(k) arrangements::combinations(allowed_nodes, k, layout = 'list')) %>%
      unlist(recursive = FALSE)
    
    # Calculate and add scores to cache.
    for (parents_com in parent_coms) {
      parents_key <- GetParentsKey(parents_com, nodes)
      scorer$parameters$parents <- parents_com
      node_cache[[parents_key]] <- do.call(scorer$scorer, scorer$parameters)
    }
    cache[[node]] <- node_cache
  }
  
  return(cache)
}

#' Get parents key.
#' 
#' TODO: The %in% function is slow. I may need to move this to C++.
#' 
#' @param parents A character vector of the parent nodes.
#' @param nodes A character vector for all nodes.
#' 
#' @noRd
GetParentsKey <- function(parents, nodes) {
  parents_key <- paste(as.integer(nodes %in% parents), collapse = '')
  
  return(parents_key)
}
