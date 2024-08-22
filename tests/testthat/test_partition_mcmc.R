dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- DAGtoPartition(dag)
scorer <- CreateScorer(data = bnlearn::learning.test[1:10, ])

current_state <- list(
  state = partitioned_nodes,
  log_score = ScoreLabelledPartition(partitioned_nodes, scorer)
)

pmcmc <- PartitionMCMC()
testthat::test_that('PartitionMCMC returns same structured state for DefaultProposal', {
  testthat::expect_identical(
    names(pmcmc(current_state, scorer)),
    c(names(current_state), 'proposal_info', 'mcmc_info')
  )
})

pmcmc <- PartitionMCMC(proposal = PartitionSplitJoin)
testthat::test_that('PartitionMCMC returns same structured state for PartitionSplitJoin', {
  testthat::expect_identical(
    names(pmcmc(current_state, scorer)),
    c(names(current_state), 'mcmc_info')
  )
})

pmcmc <- PartitionMCMC(proposal = NodeMove)
testthat::test_that('PartitionMCMC returns same structured state for NodeMove', {
  testthat::expect_identical(
    names(pmcmc(current_state, scorer)),
    c(names(current_state), 'mcmc_info')
  )
})

pmcmc <- PartitionMCMC(proposal = SwapNode)
testthat::test_that('PartitionMCMC returns same structured state for SwapNode', {
  testthat::expect_identical(
    names(pmcmc(current_state, scorer)),
    c(names(current_state), 'mcmc_info')
  )
})

pmcmc <- PartitionMCMC(proposal = StayStill)
testthat::test_that('PartitionMCMC returns same structured state for StayStill', {
  testthat::expect_identical(
    names(pmcmc(current_state, scorer)),
    c(names(current_state), 'mcmc_info')
  )
})
