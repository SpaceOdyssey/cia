# Causal Inference Assistant (cia)

This package is for performing causal inference assuming the causal process follows a directed acyclic graphs. It includes functionality to learn the structure (using partition MCMC) along with building Bayesian networks and performing probabilistic queries (using gRain).

The bulk of this package is an implementation of partition Markov Chain Monte Carlo (PMCMC) algorithm in R. Our PMCMC is similar to the BiDAG implementation but the scoring function defaults to using bnlearn which allows for a range of scoring assumptions and priors for pairwise edge probabilities. There is also more exposure of the sampling procedure itself, whereby the algorithm can return both partitions and DAGs while providing acceptance rates per proposal to understand how well we are sampling in the partition space.

Please see the documentation for more details.

## Installation

You should be able to install directly from GitHub using the following:

devtools::install_github("SpaceOdyssey/cia")

## Use

There is function documentation in the root directory along with some simple examples in the vignettes.
