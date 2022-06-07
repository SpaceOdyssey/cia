set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

testthat::test_that('SwapNode proposal has attributes', {
  testthat::expect_identical(
    names(SwapNode(partitioned_nodes)), 
    c('state', 'current_nbd', 'new_nbd')
  )
})

testthat::test_that('SwapNode proposal has same dimensions', {
  testthat::expect_identical(
    dim(ProposeSwapNode(partitioned_nodes)),
    dim(partitioned_nodes)
  )
})

testthat::test_that('SwapNode proposal has same number of elements', {
  testthat::expect_equal(
    GetNumberOfPartitions(ProposeSwapNode(partitioned_nodes)),
    GetNumberOfPartitions(partitioned_nodes)
  )
})

testthat::test_that('SwapNode proposal is different', {
  testthat::expect_true(
    sum(ProposeSwapNode(partitioned_nodes)$node != partitioned_nodes$node) > 0
  )
})
