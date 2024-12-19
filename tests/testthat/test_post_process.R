set.seed(1)
data <- bnlearn::learning.test
scorer <- CreateScorer(data = data)
init_state <- InitPartition(colnames(data), scorer, n_parallel_chains = 1)

chain <- SampleChain(10, init_state, PartitionMCMC(), scorer)
test_that('SampleChainDAGs Single', {
  testthat::expect_equal(length(PartitiontoDAG(chain, scorer)), 2)
})

chains <- SampleChains(10, init_state, PartitionMCMC(), scorer)
test_that('SampleChainDAGs Multiple', {
  testthat::expect_equal(length(PartitiontoDAG(chains, scorer)), length(chains))
})

test_that('PostProcessChains', {
  testthat::expect_equal(length(PostProcessChains(chains, n_burnin = 2, n_thin = 2)), 
                         length(chains))
})

ls <- PostProcessChains(chains, n_burnin = 2, n_thin = 2)[[1]]$log_score
test_that('PostProcessChains object have same length', {
  testthat::expect_true(length(ls) < length(chains[[1]]$log_score))
})

flat_chains <- FlattenChains(chains)
test_that(
  'Flattened chains should include chain attributes', {
    testthat::expect_true(
      length(
        setdiff(c('state', 'log_score', 'proposal_info', 'mcmc_info'),
                names(flat_chains))
        ) == 0
    )
  }
)

accept_rate <- CalculateAcceptanceRates(chains)
test_that('CalculateAcceptanceRates works', {
  testthat::expect_true(accept_rate$mean_accept <= 1.0)
  testthat::expect_true(accept_rate$n_accept <= CalculateAcceptanceRates(chains)$n_total)
})

nodes <- c('A', 'B')
dag <- matrix(c(0, 1, 0, 0), nrow = 2, dimnames = list(nodes, nodes))
test_that("DAGToCPDAG returns appropriate CPDAG", {
  testthat::expect_true(isSymmetric(DAGtoCPDAG(dag)))
})
