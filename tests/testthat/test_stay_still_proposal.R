set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

testthat::test_that('StayStill has attributes', {
  expect_identical(
    setdiff(names(StayStill(partitioned_nodes)), c('state', 'current_nbd', 'new_nbd')),
    character(0)
  )
})

testthat::test_that('StayStill proposes the same labelled partition', {
  testthat::expect_identical(
    ProposeStayStill(partitioned_nodes), partitioned_nodes
  )
})
