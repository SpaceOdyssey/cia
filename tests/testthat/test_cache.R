data <- bnlearn::learning.test
scorer <- CreateScorer(data = data)
cache <- BuildCache(scorer)
cached_scorer <- CachedScorer(scorer)

testthat::test_that('CachedScorer returns function', {
  testthat::expect_true(methods::is(cached_scorer, 'function'))
})

testthat::test_that('Cached scorer looks up correct score', {
  testthat::expect_true(cached_scorer('A', c('B', 'C')) < 0.0)
  testthat::expect_true(cached_scorer('A', c()) < 0.0)
  testthat::expect_true(cached_scorer('A', NULL) < 0.0)
  testthat::expect_equal(cached_scorer('A', c('B', 'C')),
                         BNLearnScorer('A', c('B', 'C'), data = data))
  testthat::expect_equal(cached_scorer('A', c()),
                         BNLearnScorer('A', c(), data = data))  
})
