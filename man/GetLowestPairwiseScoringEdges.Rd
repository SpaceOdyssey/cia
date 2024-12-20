% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_process.R
\name{GetLowestPairwiseScoringEdges}
\alias{GetLowestPairwiseScoringEdges}
\title{Preprocessing for blacklisting
Get the lowest pairwise scoring edges.}
\usage{
GetLowestPairwiseScoringEdges(scorer, n_retain)
}
\arguments{
\item{scorer}{A scorer object.}

\item{n_retain}{An integer representing the number of edges to retain.}
}
\value{
A boolean matrix of (parent, child) pairs for blacklisting.
}
\description{
Get the lowest pairwise scoring edges represented as a blacklist matrix.
This blacklisting procedure is motivated by Koller & Friedman (2003). This
is rarely used now as we found that it blacklists edges that have significant
dependencies but are not in the top \eqn{n} edges. We prefer
the GetIncrementalScoringEdges method.
}
\examples{
data <- bnlearn::learning.test

scorer <- CreateScorer(
  scorer = BNLearnScorer, 
  data = data
  )
  
blacklist <- GetLowestPairwiseScoringEdges(scorer, 3)

blacklist_scorer <- CreateScorer(
  scorer = BNLearnScorer, 
  data = data,
  blacklist = blacklist,
  cache = TRUE
  )

init_state <- InitPartition(colnames(data), blacklist_scorer)

results <- SampleChains(10, init_state, PartitionMCMC(), blacklist_scorer)

}
\references{
\enumerate{
\item Koller D, Friedman N. Being Bayesian about network structure. A Bayesian
approach to structure discovery in Bayesian networks. Mach Learn.
2003;50(1):95–125.
}
}
