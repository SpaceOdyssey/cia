data <- bnlearn::learning.test
scorer <- CreateScorer(data = data)

scores <- CalculatePairwiseScores(scorer)
test_that("CalculatePairwiseScores return legitimate scores", {
  testthat::expect_true(all(scores[upper.tri(scores)] < 0.0))
  testthat::expect_true(all(scores[lower.tri(scores)] < 0.0))
})

n_retain <- 2
blacklist <- GetLowestPairwiseScoringEdges(scorer, n_retain)
test_that("CalculatePairwiseScores return legitimate scores", {
  testthat::expect_equal(sum(!blacklist, na.rm = TRUE), n_retain*ncol(data))
})
