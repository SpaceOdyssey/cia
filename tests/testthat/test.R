test_that("UniformlySampleDAG works", {
  testthat::expect_identical(length(dim(UniformlySampleDAG (c('D', 'E', 'F')))), as.integer(2))
  testthat::expect_identical(dim(UniformlySampleDAG (c('D', 'E', 'F'))), as.integer(c(3, 3)))
  testthat::expect_identical(colnames(UniformlySampleDAG (c('G', 'H', 'I'))), c('G', 'H', 'I'))
  testthat::expect_identical(rownames(UniformlySampleDAG (c('J', 'K', 'L'))), c('J', 'K', 'L'))
})

test_that("GetEmptyDAG works", {
  testthat::expect_identical(length(dim(GetEmptyDAG (c('D', 'E', 'F')))), as.integer(2))
  testthat::expect_identical(dim(GetEmptyDAG (c('D', 'E', 'F'))), as.integer(c(3, 3)))
  testthat::expect_identical(colnames(GetEmptyDAG (c('G', 'H', 'I'))), c('G', 'H', 'I'))
  testthat::expect_identical(rownames(GetEmptyDAG (c('J', 'K', 'L'))), c('J', 'K', 'L'))
})

data <- bnlearn::learning.test
test_that("BNLearn scorer works", {
  testthat::expect_true(BNLearnScorer('A', c('B', 'C'), data = data) < 0.0)
  testthat::expect_true(BNLearnScorer('A', c(), data = data) < 0.0)
  testthat::expect_true(BNLearnScorer('A', vector(), data = data) < 0.0)
  testthat::expect_true(
    BNLearnScorer(
      'A', c('B', 'C'), 
      data = data, type = 'bde', iss = 100
      ) < BNLearnScorer(
        'A', c('B', 'C'), 
        data = data, type = 'bde', iss = 1)
    )
})

scorer_1 <- list(
  scorer = dagmc::BNLearnScorer, 
  parameters = list(data = data)
  )
scorer_2 <- list(
  scorer = dagmc::BNLearnScorer, 
  parameters = list(data = data, type = 'bde', iss = 1)
  )
test_that('ScoreTableNode works', {
  testthat::expect_true(sum(ScoreTableNode('A', c('B', 'C'), scorer_1)$log_scores) < 0.0)
  testthat::expect_true(sum(ScoreTableNode('A', c(), scorer_1)$log_scores) < 0.0)
  testthat::expect_true(sum(ScoreTableNode('A', NULL, scorer_1)$log_scores) < 0.0)              
})

lscores <- length(ScoreTableNode('A', c('B', 'C'), scorer_1)$log_scores)
lpa <- length(ScoreTableNode('A', c('B', 'C'), scorer_1)$parent_combinations)
test_that('Lengths of ScoreTableNode elements match', {
  testthat::expect_equal(lscores, lpa)
})

test_that('ScoreNode works', {
  testthat::expect_true(ScoreNode('A', c('B', 'C'), scorer_1) < 0.0)
  testthat::expect_true(ScoreNode('A', c('B', 'C'), scorer_2) < 0.0)
})

dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)
test_that('ScoreLabelledPartition works', {
  testthat::expect_true(ScoreLabelledPartition(partitioned_nodes, scorer_1) < 0.0)
  testthat::expect_true(ScoreLabelledPartition(partitioned_nodes, scorer_2) < 0.0)
})

testthat::test_that('Step SplitJoin proposal', {
  testthat::expect_identical(
    dim(ProposePartitionSplitJoin(partitioned_nodes)),
    dim(partitioned_nodes)
  )
})

new_dag <- UniformlySampleDAG(colnames(data))
new_partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(new_dag)
testthat::test_that('ScoreDiff works', {
  testthat::expect_equal(
    ScoreDiff(partitioned_nodes, new_partitioned_nodes, scorer = scorer_1),
    ScoreLabelledPartition(new_partitioned_nodes, scorer_1) - ScoreLabelledPartition(partitioned_nodes, scorer_1)
  )
})

current_state <- list(
  state = partitioned_nodes,
  log_score = ScoreLabelledPartition(partitioned_nodes, scorer_1)
)
testthat::test_that('PartitionMCMC step works', {
  testthat::expect_identical(
    names(PartitionMCMC(current_state, proposal = ProposePartitionSplitJoin, scorer = scorer_1)),
    names(current_state)
    )
})

testthat::test_that('SampleChain works', {
  testthat::expect_equal(
    length(
      SampleChain(10, partitioned_nodes, PartitionMCMC,
                  proposal = ProposePartitionSplitJoin, scorer = scorer_1)),
      2
  )
})

chain <- SampleChain(100, partitioned_nodes, PartitionMCMC,
                     proposal = ProposePartitionSplitJoin, scorer = scorer_1)
testthat::test_that('Check SampleChain summary', {
  testthat::expect_equal(chain$log_score[[100]],
                         ScoreLabelledPartition(chain$state[[100]], scorer_1) 
                         )
})

