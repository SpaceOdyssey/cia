data <- bnlearn::learning.test
scorer <- CreateScorer(data = data)

scores <- CalculatePairwiseScores(scorer)
test_that("CalculatePairwiseScores return legitimate scores", {
  testthat::expect_true(all(scores[upper.tri(scores)] < 0.0))
  testthat::expect_true(all(scores[lower.tri(scores)] < 0.0))
})

c <- 2
blacklist <- GetLowestScoringEdges(scorer, c)
test_that("CalculatePairwiseScores return legitimate scores", {
  testthat::expect_equal(sum(!blacklist, na.rm = TRUE), c*ncol(data))
})
