# dagmcmc

Implementations of Markov Chain Monte Carlo methods in R.

## Current State

I am prototyping the partition MCMC algorithm (Kuipers & Moffa, 2015). Ultimate aim will be to include other MCMC algorithms such as OrderMCMC, structureMCMC, etc.

## R Prerequisites

bnlearn\
igraph

## Use

The current use is as follows:
1) Create a scorer. This is implemented as a list with elements; scorer (which is a callable) and parameters (which are passed to the scorer callable). BNLearnScorer is the only scorer caller available, which is a wrapper on the bnlearn::score function.
2) Create a proposal. This will be a function that takes the current state and outputs a new state based on a proposal algorithm. ProposePartitionSplitJoin, which performs algorithm 1 in Kuipers & Moffa (2015), is the only one available at the moment.
3) There are then two options to draw a sample(s) given the current state; A) use PartitionMCMC which takes the current state, proposal and scorer then outputs a new state, or 2) use SampleChain to get n_results by running the MCMC for multiple steps.
4) The 'states' are currently represented by a labelled partition (a data.frame of (node, partition element) pairs). To draw a DAG from a partition you need to run SampleDAGFromLabelledPartition. You need to run this as a post-process after running PartitionMCMC or SampleChain. As this effectively has to rescore the (node, parents) combinations again, implementing this as a post-process may be quite inefficient. Happy for new ideas here.
5) As the states are represented by labelled partitions, you may need to convert a DAG into a labelled partition. GetPartitionedNodesFromAdjacencyMatrix currently does this.
6) There are a number of other helper functions that may be a little difficult to navigate at the moment. I will split the prototype into appropriate scripts to make it a little easier to navigate.
