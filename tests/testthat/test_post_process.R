data <- bnlearn::learning.test

set.seed(1)
dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

scorer <- CreateScorer(data = data)

chain <- SampleChain(10, partitioned_nodes, PartitionMCMC(), scorer)
test_that("SampleChainDAGs Single", {
  testthat::expect_equal(length(SampleChainDAGs(chain, scorer)), length(chain) + 2)
})

chains <- SampleChains(10, partitioned_nodes, PartitionMCMC(), scorer)
test_that("SampleChainDAGs Multiple", {
  testthat::expect_equal(length(SampleChainDAGs(chains, scorer)), length(chains))
})

test_that('PostProcessChains', {
  testthat::expect_equal(length(PostProcessChains(chains, n_burnin = 2, n_thin = 2)), 
                         length(chains))
})

ls <- PostProcessChains(chains, n_burnin = 2, n_thin = 2)[[1]]$log_score
test_that('PostProcessChains object have same length', {
  testthat::expect_true(length(ls) < length(chains[[1]]$log_score))
})

accept_rate <- CalculateAcceptanceRates(chains)
test_that('CalculateAcceptanceRates works', {
  testthat::expect_true(accept_rate$mean_accept <= 1.0)
  testthat::expect_true(accept_rate$n_accept <= CalculateAcceptanceRates(chains)$n_total)
})
