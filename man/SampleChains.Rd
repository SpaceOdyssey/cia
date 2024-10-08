% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_chain.R
\name{SampleChains}
\alias{SampleChains}
\title{Sample multiple chains in parallel.}
\usage{
SampleChains(
  n_results,
  init_state,
  transition,
  scorer,
  n_thin = 1,
  n_parallel_chains = 2
)
}
\arguments{
\item{n_results}{Number of saved states per chain.}

\item{init_state}{An initial state that can be passed to transition. This can
be a single state or a list of states for each parallel chain.}

\item{transition}{A transition function.}

\item{scorer}{A scorer object.}

\item{n_thin}{Number of steps between saved states.}

\item{n_parallel_chains}{Number of chains to run in parallel. Default is 2.}
}
\value{
chains A cia_chains object.
}
\description{
Sample multiple chains in parallel.
}
\examples{
data <- bnlearn::learning.test

dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- DAGtoPartition(dag)

scorer <- list(
  scorer = BNLearnScorer, 
  parameters = list(data = data)
  )

results <- SampleChains(10, partitioned_nodes, PartitionMCMC(), scorer)

}
