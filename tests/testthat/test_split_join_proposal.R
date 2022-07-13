dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

testthat::test_that('SplitJoin proposal has attributes', {
  testthat::expect_identical(
    names(PartitionSplitJoin(partitioned_nodes)), 
    c('state', 'current_nbd', 'new_nbd', 'rescore_nodes'))
})

testthat::test_that('SplitJoin proposal has same dimensions', {
  testthat::expect_identical(
    names(ProposePartitionSplitJoin(partitioned_nodes)),
    c('partitioned_nodes', 'rescore_nodes')
  )
})

scorer <- CreateScorer(data = bnlearn::learning.test)
proposed <- ProposePartitionSplitJoin(partitioned_nodes)
testthat::test_that('ScoreDiff is correct', {
  testthat::expect_equal(
    ScoreLabelledPartition(proposed$partitioned_nodes, scorer) - ScoreLabelledPartition(partitioned_nodes, scorer),
    ScoreDiff(partitioned_nodes, proposed$partitioned_nodes, 
              scorer, proposed$rescore_nodes)
  )
})
