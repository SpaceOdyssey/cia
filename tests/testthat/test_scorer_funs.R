data <- bnlearn::learning.test
scorer_1 <- list(
  scorer = dagmc::BNLearnScorer, 
  parameters = list(data = data)
)
scorer_2 <- list(
  scorer = dagmc::BNLearnScorer, 
  parameters = list(data = data, type = 'bde', iss = 1)
)
testthat::test_that('ScoreTableNode works', {
  testthat::expect_true(sum(ScoreTableNode('A', c('B', 'C'), scorer_1)$log_scores) < 0.0)
  testthat::expect_true(sum(ScoreTableNode('A', c(), scorer_1)$log_scores) < 0.0)
  testthat::expect_true(sum(ScoreTableNode('A', NULL, scorer_1)$log_scores) < 0.0)              
})

lscores <- length(ScoreTableNode('A', c('B', 'C'), scorer_1)$log_scores)
lpa <- length(ScoreTableNode('A', c('B', 'C'), scorer_1)$parent_combinations)
testthat::test_that('Lengths of ScoreTableNode elements match', {
  testthat::expect_equal(lscores, lpa)
})

testthat::test_that('ScoreNode works', {
  testthat::expect_true(ScoreNode('A', c('B', 'C'), scorer_1) < 0.0)
  testthat::expect_true(ScoreNode('A', c('B', 'C'), scorer_2) < 0.0)
})

set.seed(1)
dag <- UniformlySampleDAG(c('A', 'B', 'C', 'D', 'E', 'F'))
bn_dag <- bnlearn::empty.graph(colnames(data))
bnlearn::amat(bn_dag) <- dag
testthat::test_that('Check ScoreDAG with BNLearn against bnlearn::score', {
  testthat::expect_equal(ScoreDAG(dag, scorer_1), bnlearn::score(bn_dag, data = data))
})

set.seed(1)
dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
test_that('ScoreLabelledPartition are less than 0', {
  testthat::expect_true(ScoreLabelledPartition(partitioned_nodes, scorer_1) < 0.0)
  testthat::expect_true(ScoreLabelledPartition(partitioned_nodes, scorer_2) < 0.0)
})

new_dag <- UniformlySampleDAG(colnames(data))
new_partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(new_dag)
testthat::test_that('ScoreDiff is consistent', {
  testthat::expect_equal(
    ScoreDiff(partitioned_nodes, new_partitioned_nodes, scorer = scorer_1),
    ScoreLabelledPartition(new_partitioned_nodes, scorer_1) - ScoreLabelledPartition(partitioned_nodes, scorer_1)
  )
})