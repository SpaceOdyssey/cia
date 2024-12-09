% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_process.R
\name{PostProcessChains}
\alias{PostProcessChains}
\title{Index chains for further analysis}
\usage{
PostProcessChains(chains, n_burnin = 0, n_thin = 1)
}
\arguments{
\item{chains}{cia_chain(s) object.}

\item{n_burnin}{Number of steps to remove at the start as a burnin. Default
is 0.}

\item{n_thin}{Number of steps between retained states. Default is 1.}
}
\value{
A cia_chain(s) object.
}
\description{
This allows you to remove a burnin and thin the chains after processing. This
is mostly redundant as you can now index the cia_chain(s) objects directly.
}
\examples{
data <- bnlearn::learning.test

dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- DAGtoPartition(dag)

scorer <- CreateScorer(
  scorer = BNLearnScorer,
  data = data
  )

results <- SampleChains(100, partitioned_nodes, PartitionMCMC(), scorer)
thinned_results <- PostProcessChains(results, n_thin = 2)

}
