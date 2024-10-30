#' Convert to a gRain object.
#' 
#' @param x An adjacency matrix or igraph object.
#' @param ... extra parameters to gRain compile.
#' 
#' @returns A gRain object.
#' 
#' @examples
#' dag <- bnlearn::model2network("[A][C][F][B|A][D|A:C][E|B:F]")
#' gRain_obj <- togRain(x = dag |> toMatrix(), data = bnlearn::learning.test)
#' 
#' @export
togRain <- function(x, ...) UseMethod("togRain")

#' @export
togRain.default <- function(x, 
                            control = list(), 
                            smooth = 0, 
                            compile =  TRUE,
                            details = 0,
                            data = NULL,
                            ...
){
  togRain.igraph(x, 
                 control = list(), 
                 smooth = 0, 
                 compile =  TRUE,
                 details = 0,
                 data = NULL,
                 ...
  )
}

#' @export
togRain.igraph <- function(x, 
                           control = list(), 
                           smooth = 0, 
                           compile =  TRUE,
                           details = 0,
                           data = NULL,
                           ...
) {
  gRbase::compile(gRain::grain(x, control, smooth, compile, details, data, ...))
}

#' @export
togRain.bn <- function(x, 
                       control = list(),
                       smooth = 0,
                       compile = TRUE,
                       details = 0,
                       data = NULL,
                       ...
) {
  igraph_obj <- igraph::as.igraph(x)
  togRain(igraph_obj, control, smooth, compile, details, data, ...)
}

#' @export  
togRain.matrix <- function(x, 
                           control = list(),
                           smooth = 0,
                           compile = TRUE,
                           details = 0,
                           data = NULL,
                           ...
) {
  igraph_obj <- igraph::graph_from_adjacency_matrix(x)
  togRain(igraph_obj, control, smooth, compile, details, data, ...)
}

