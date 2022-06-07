test_that("UniformlySampleDAG works", {
  testthat::expect_identical(length(dim(UniformlySampleDAG(c('D', 'E', 'F')))), as.integer(2))
  testthat::expect_identical(dim(UniformlySampleDAG(c('D', 'E', 'F'))), as.integer(c(3, 3)))
  testthat::expect_identical(colnames(UniformlySampleDAG(c('G', 'H', 'I'))), c('G', 'H', 'I'))
  testthat::expect_identical(rownames(UniformlySampleDAG(c('J', 'K', 'L'))), c('J', 'K', 'L'))
})

test_that("GetEmptyDAG works", {
  testthat::expect_identical(length(dim(GetEmptyDAG(c('D', 'E', 'F')))), as.integer(2))
  testthat::expect_identical(dim(GetEmptyDAG(c('D', 'E', 'F'))), as.integer(c(3, 3)))
  testthat::expect_identical(colnames(GetEmptyDAG(c('G', 'H', 'I'))), c('G', 'H', 'I'))
  testthat::expect_identical(rownames(GetEmptyDAG(c('J', 'K', 'L'))), c('J', 'K', 'L'))
})
