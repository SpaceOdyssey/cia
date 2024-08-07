% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/propose_swap_adjacent.R
\name{ProposeSwapAdjacentNode}
\alias{ProposeSwapAdjacentNode}
\title{Propose that two nodes swap partition elements.}
\usage{
ProposeSwapAdjacentNode(partitioned_nodes)
}
\arguments{
\item{partitioned_nodes}{labelled partition.}
}
\value{
A proposed labelled partition.
}
\description{
Propose that two nodes swap partition elements.
}
\examples{
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
ProposeStayStill(partitioned_nodes)

}
