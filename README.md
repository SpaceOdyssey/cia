# dagmc

An implementation of partition Markov Chain Monte Carlo (PMCMC) algorithm in R. Our PMCMC is similar to the BiDAG implementation but the scoring function defaults to using bnlearn which allows for a range of scoring assumptions and prior edge probabilities. There is also more exposure of the sampling procedure itself, whereby the algorithm returns partitions rather than DAGs and provides acceptance rates per proposal to understand how well we are sampling in the partition space.

There is also some simple functionality to help perform probabilistic queries within gRain. Please see the documentation for more details.

## Installation

You should be able to install directly from GitHub using the following:

devtools::install_github("mvar0005/dagmc", host="github.sydney.edu.au/api/v3", auth_token = 'auth_token')

You can get an 'auth_token' by following the instructions at:

https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token

## Use

There is function documentation in the root directory. You can find some simple examples in the vignettes.
