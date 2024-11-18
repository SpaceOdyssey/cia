
# Causal Inference Assistant (cia)

<!-- badges: start -->
[![R-CMD-check](https://github.com/SpaceOdyssey/cia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SpaceOdyssey/cia/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/cia)](https://CRAN.R-project.org/package=cia)
[![codecov](https://codecov.io/github/SpaceOdyssey/cia/graph/badge.svg?token=NELX4A88RT)](https://app.codecov.io/github/SpaceOdyssey/cia)
<!-- badges: end -->

This package is for performing causal structure learning and inference
assuming the causal process follows a directed acyclic graph (DAG). It
includes functionality to learn the structure using partition MCMC along
with building Bayesian networks and performing probabilistic queries
(using gRain).

The bulk of this package is an implementation of partition Markov Chain
Monte Carlo (PMCMC) algorithm in R. Our PMCMC is similar to the
[BiDAG](https://cran.r-project.org/package=BiDAG) implementation but the scoring
function defaults to using [bnlearn](https://www.bnlearn.com/) which allows for
a range of scoring assumptions and priors for pairwise edge probabilities. There
is also more exposure of the sampling procedure itself, whereby the algorithm
can return both partitions and DAGs while providing convergence diagnostics to
understand how well the algorithm is sampling in both partition and DAG space.

We provide a simple
[example](https://spaceodyssey.github.io/cia/articles/three_node_example.html)
and function
[documentation](https://spaceodyssey.github.io/cia/reference/index.html).

## Installation

Install the released version of cia from CRAN:

``` r
install.packages("cia")
```

Or, install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("SpaceOdyssey/cia")
```
