set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

testthat::test_that('NodeMove has attributes', {
  expect_identical(
    setdiff(names(NodeMove(partitioned_nodes)), c('state', 'current_nbd', 'new_nbd')),
    character(0)
  )
})

testthat::test_that('NodeMovement proposal has same dimensions', {
  testthat::expect_identical(
    dim(ProposeNodeMove(partitioned_nodes)),
    dim(partitioned_nodes)
  )
})

new_node_el <- ProposeNodeMove(partitioned_nodes)
testthat::test_that('NodeMovement proposal meets conditions', {
  testthat::expect_true(max(new_node_el$partition) >= max(partitioned_nodes$partition))
  testthat::expect_true(length(unique(new_node_el$partition)) >= length(unique(partitioned_nodes$partition)))
  testthat::expect_equal(min(new_node_el$partition), 1)
})


  