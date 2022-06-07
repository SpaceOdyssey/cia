set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

scorer_1 <- list(
  scorer = dagmc::BNLearnScorer, 
  parameters = list(data = bnlearn::learning.test[1:10, ])
)

current_state <- list(
  state = partitioned_nodes,
  log_score = ScoreLabelledPartition(partitioned_nodes, scorer_1)
)

testthat::test_that('PartitionMCMC returns same structured state', {
  testthat::expect_identical(
    names(PartitionMCMC(current_state, proposal = PartitionSplitJoin, scorer = scorer_1)),
    names(current_state)
  )
})
