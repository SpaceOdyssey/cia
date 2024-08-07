% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/propose_split_join.R
\name{CalculateSplitJoinNeighbourhood}
\alias{CalculateSplitJoinNeighbourhood}
\title{Calculate neighbourhood for the split or join proposal.}
\usage{
CalculateSplitJoinNeighbourhood(partitioned_nodes)
}
\arguments{
\item{partitioned_nodes}{Labelled partition.}
}
\description{
The number of split combinations prescribed by KP15 is ambiguous when a
partition element has only 1 node. A split for a partition element with 1
node results in a proposal to stay still, as such I remove that proposal.
}
