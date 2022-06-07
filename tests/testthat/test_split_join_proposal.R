set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

testthat::test_that('SplitJoin proposal has attributes', {
  testthat::expect_identical(
    names(PartitionSplitJoin(partitioned_nodes)), 
    c('state', 'current_nbd', 'new_nbd'))
})

testthat::test_that('SplitJoin proposal has same dimensions', {
  testthat::expect_identical(
    dim(ProposePartitionSplitJoin(partitioned_nodes)),
    dim(partitioned_nodes)
  )
})
