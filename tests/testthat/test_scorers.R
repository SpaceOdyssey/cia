data <- bnlearn::learning.test
testthat::test_that('BNLearn scorer works', {
  testthat::expect_true(BNLearnScorer('A', c('B', 'C'), data = data) < 0.0)
  testthat::expect_true(BNLearnScorer('A', c(), data = data) < 0.0)
  testthat::expect_true(BNLearnScorer('A', vector(), data = data) < 0.0)
  testthat::expect_true(
    BNLearnScorer(
      'A', c('B', 'C'), data = data, type = 'bde', iss = 100
      ) > BNLearnScorer(
      'A', c('B', 'C'), data = data, type = 'bde', iss = 1)
  )
})
