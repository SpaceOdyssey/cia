data <- bnlearn::learning.test

set.seed(1)
dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

scorer_1 <- list(
  scorer = BNLearnScorer, 
  parameters = list(data = data)
)

testthat::test_that('SampleChain returns consistent state dimensions for DefaultProposal', {
  testthat::expect_equal(
    length(
      SampleChain(10, partitioned_nodes, PartitionMCMC,
                  proposal = DefaultProposal(), scorer = scorer_1)),
      4
  )
})

testthat::test_that('SampleChain returns consistent state dimensions for PartitionSplitJoin proposal', {
  testthat::expect_equal(
    length(
      SampleChain(10, partitioned_nodes, PartitionMCMC,
                  proposal = PartitionSplitJoin, scorer = scorer_1)),
    4
  )
})

testthat::test_that('SampleChain returns consistent state dimensions for NodeMove proposal', {
  testthat::expect_equal(
    length(
      SampleChain(10, partitioned_nodes, PartitionMCMC,
                  proposal = NodeMove, scorer = scorer_1)),
    4
  )
})

testthat::test_that('SampleChain returns consistent state dimensions for SwapNode proposal', {
  testthat::expect_equal(
    length(
      SampleChain(10, partitioned_nodes, PartitionMCMC,
                  proposal = SwapNode, scorer = scorer_1)),
    4
  )
})

set.seed(1)
chain <- SampleChain(100, partitioned_nodes, PartitionMCMC,
                     proposal = DefaultProposal(), scorer = scorer_1)
testthat::test_that('Check SampleChain returns correct scores', {
  testthat::expect_equal(chain$log_score[[100]],
                         ScoreLabelledPartition(chain$state[[100]], scorer_1) 
                         )
})
