data <- bnlearn::learning.test

set.seed(1)
dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

scorer_1 <- list(
  scorer = BNLearnScorer, 
  parameters = list(data = data)
)

testthat::test_that('SampleDAGFromLabelledPartition works', {
  testthat::expect_equal(colnames(SampleDAGFromLabelledPartition(partitioned_nodes, scorer_1)), colnames(data))
  testthat::expect_equal(rownames(SampleDAGFromLabelledPartition(partitioned_nodes, scorer_1)), colnames(data))
})
