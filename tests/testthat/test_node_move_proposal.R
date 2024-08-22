set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- DAGtoPartition(dag)

testthat::test_that('NodeMove has attributes', {
  expect_identical(
    names(NodeMove(partitioned_nodes)), 
    c('state', 'current_nbd', 'new_nbd', 'rescore_nodes')
  )
})

testthat::test_that('NodeMovement proposal has same dimensions', {
  testthat::expect_identical(
    names(ProposeNodeMove(partitioned_nodes)),
    c('partitioned_nodes', 'rescore_nodes')
  )
})

new_node_el <- ProposeNodeMove(partitioned_nodes)$partitioned_nodes
testthat::test_that('NodeMovement proposal meets conditions', {
  testthat::expect_equal(min(new_node_el$partition), 1)
})
