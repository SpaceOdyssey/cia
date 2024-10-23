# Causal Inference Assistant (cia)

This package is for performing causal structure learning and inference assuming the causal process follows a directed acyclic graph. It includes functionality to learn the structure using partition MCMC along with building Bayesian networks and performing probabilistic queries (using gRain).

The bulk of this package is an implementation of partition Markov Chain Monte Carlo (PMCMC) algorithm in R. Our PMCMC is similar to the BiDAG implementation but the scoring function defaults to using bnlearn which allows for a range of scoring assumptions and priors for pairwise edge probabilities. There is also more exposure of the sampling procedure itself, whereby the algorithm can return both partitions and DAGs while providing acceptance rates per proposal to understand how well we are sampling in the partition space.

Please see the documentation for more details.

## Installation

You should be able to install directly from GitHub using the following:

devtools::install_github("SpaceOdyssey/cia")

## Use

We provide a simple [example](https://spaceodyssey.github.io/cia/articles/three_node_example.html) and function [documentation](https://spaceodyssey.github.io/cia/reference/index.html).

[![R-CMD-check](https://github.com/SpaceOdyssey/cia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SpaceOdyssey/cia/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/SpaceOdyssey/cia/graph/badge.svg?token=NELX4A88RT)](https://codecov.io/github/SpaceOdyssey/cia)
