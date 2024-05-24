#' This creates a mutilated graph in accordance with an intervention.
#' 
#' @param grain_object A grain object.
#' @param intervention A list of nodes and their corresponding intervention
#' distribution represented as a vector of unconditional probabilities.
#' 
#' @return A grain object. Please note that any evidence set within the grain 
#' object will not be passed to the new object.
#' 
#' @examples
#' # This creates a mutilated graph in accordance with turning the sprinkler 
#' # on in the wet grass example (i.e, do(S = 'yes')).
#' yn <- c("yes", "no")
#' p.R <- gRain::cptable(~R, values=c(.2, .8), levels=yn)
#' p.S_R <- gRain::cptable(~S:R, values=c(.01, .99, .4, .6), levels=yn)
#' p.G_SR <- gRain::cptable(~G:S:R, values=c(.99, .01, .8, .2, .9, .1, 0, 1), levels=yn)
#' wet.cpt <- gRain::grain(gRain::compileCPT(p.R, p.S_R, p.G_SR))
#' 
#' mut_graph <- MutilateGraph(wet.cpt, list(S = c(1.0, 0.0)))
#' 
#' # You can then use querygrain to perform an intervention query. For example,
#' # p(G | do(S = 'yes')) is given by,
#' gRain::querygrain(mut_graph, 'G')
#' 
#' # You can also perform an observational query for a node not affected
#' # by the intervention. For example, p(R | do(S = 'yes')) = p(R) is given by,
#' gRain::querygrain(mut_graph, 'R')
#' 
#' @export
MutilateGraph <- function(grain_object, intervention) {
  
  # Create mutilated graph by iteratively replacing the conditional probability
  # tables.
  intervention_nodes <- names(intervention)
  for (node in intervention_nodes) {
    
    node_nlevels <- length(grain_object$universe$levels[[node]])
    
    times <- gRain::getgrain(grain_object, 'cpt')[[node]] |> 
      dim() |> 
      prod()/node_nlevels
    node_intervention <- rep(intervention[[node]], times = times)
    
    cpt <- list()
    cpt[[node]] <- node_intervention
    grain_object <- gRain::replaceCPT(grain_object, cpt)
  }
  
  return(grain_object)
}
