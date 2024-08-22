data <- bnlearn::learning.test

set.seed(1)
dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- DAGtoPartition(dag)

scorer <- CreateScorer(data = data)

testthat::test_that('SampleChain returns consistent state dimensions for DefaultProposal', {
  testthat::expect_equal(
    length(SampleChain(10, partitioned_nodes, PartitionMCMC(), scorer)), 4
  )
})

testthat::test_that('SampleChain returns consistent state dimensions for PartitionSplitJoin proposal', {
  testthat::expect_equal(
    length(SampleChain(10, partitioned_nodes, PartitionMCMC(proposal = PartitionSplitJoin), scorer = scorer)),
    4
  )
})

testthat::test_that('SampleChain returns consistent state dimensions for NodeMove proposal', {
  testthat::expect_equal(
    length(SampleChain(10, partitioned_nodes, PartitionMCMC(proposal = NodeMove), scorer = scorer)),
    4
  )
})

testthat::test_that('SampleChain returns consistent state dimensions for SwapNode proposal', {
  testthat::expect_equal(
    length(SampleChain(10, partitioned_nodes, PartitionMCMC(proposal = SwapNode), scorer = scorer)),
    4
  )
})

testthat::test_that('SampleChains returns a list of chains', {
  testthat::expect_equal(
    length(SampleChains(10, partitioned_nodes, PartitionMCMC(proposal = SwapNode), scorer = scorer)),
    2
  )
})

set.seed(1)
chain <- SampleChain(100, partitioned_nodes, PartitionMCMC(), scorer = scorer)
testthat::test_that('Check SampleChain returns correct scores', {
  testthat::expect_equal(chain$log_score[[100]], 
                         ScoreLabelledPartition(chain$state[[100]], scorer) 
  )
})

blacklist <- GetLowestPairwiseScoringEdges(scorer, n_retain = 2)
scorer <- CreateScorer(data = data, blacklist = blacklist)
dag[which(scorer$blacklist)] <- 0
partitioned_nodes <- DAGtoPartition(dag)
testthat::test_that('SampleChain works for blacklist', {
  testthat::expect_equal(
    length(SampleChain(100, partitioned_nodes, PartitionMCMC(), scorer = scorer)),
    4
  )
})

chain <- SampleChain(100, partitioned_nodes, PartitionMCMC(), scorer = scorer)
dags <- PartitiontoDAG(chain, scorer)
testthat::test_that('SampleChain does not return DAGs with blacklisted edges.', {
  testthat::expect_equal(
    sum(sapply(1:100, function(x) sum(simplify2array(dags$state)[, , x] * scorer$blacklist, na.rm = TRUE))),
    0)
})
