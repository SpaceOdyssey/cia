data <- bnlearn::learning.test

set.seed(1)
dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- DAGtoPartition(dag)

scorer <- CreateScorer(data = data)

testthat::test_that('SampleDAGFromLabelledPartition works', {
  testthat::expect_equal(colnames(SampleDAGFromLabelledPartition(partitioned_nodes, scorer)$state), colnames(data))
  testthat::expect_equal(rownames(SampleDAGFromLabelledPartition(partitioned_nodes, scorer)$state), colnames(data))
})
