set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- DAGtoPartition(dag)

testthat::test_that('StayStill has attributes', {
  expect_identical(
    names(StayStill(partitioned_nodes)), 
    c('state', 'current_nbd', 'new_nbd', 'rescore_nodes')
  )
})

testthat::test_that('StayStill proposes the same labelled partition', {
  testthat::expect_identical(
    names(ProposeStayStill(partitioned_nodes)), 
    c('partitioned_nodes', 'rescore_nodes')
  )
})
