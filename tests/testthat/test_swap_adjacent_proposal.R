set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

testthat::test_that('SwapAdjacentNode proposal has attributes', {
  testthat::expect_identical(
    names(SwapAdjacentNode(partitioned_nodes)), 
    c('state', 'current_nbd', 'new_nbd', 'rescore_nodes')
  )
})

testthat::test_that('SwapAdjacentNode proposal has same dimensions', {
  testthat::expect_identical(
    names(ProposeSwapAdjacentNode(partitioned_nodes)),
    c('partitioned_nodes', 'rescore_nodes')
  )
})

new_partitioned_nodes <- ProposeSwapAdjacentNode(partitioned_nodes)$partitioned_nodes
testthat::test_that('SwapAdjacentNode proposal has same number of elements', {
  testthat::expect_equal(
    GetNumberOfPartitions(new_partitioned_nodes),
    GetNumberOfPartitions(partitioned_nodes)
  )
})

testthat::test_that('SwapAdjacentNode proposal is different', {
  testthat::expect_true(
    sum(new_partitioned_nodes$node != partitioned_nodes$node) > 0
  )
})
