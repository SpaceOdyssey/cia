data <- bnlearn::learning.test

dag <- UniformlySampleDAG(names(data))
partitioned_nodes <- GetPartitionedNodesFromAdjacencyMatrix(dag)

scorer_1 <- CreateScorer(data = data)
scorer_2 <- CreateScorer(data = data, type = 'bde', iss = 1)

testthat::test_that('ScoreTableNode works', {
  testthat::expect_true(sum(ScoreTableNode(partitioned_nodes, 'A', scorer_1)$log_scores) < 0.0)
  testthat::expect_true(sum(ScoreTableNode(partitioned_nodes, 'A', scorer_2)$log_scores) < 0.0)
  testthat::expect_true(sum(ScoreTableNode(partitioned_nodes, 'B', scorer_1)$log_scores) < 0.0)
  testthat::expect_true(sum(ScoreTableNode(partitioned_nodes, 'B', scorer_2)$log_scores) < 0.0)
})

lscores <- length(ScoreTableNode(partitioned_nodes, 'A', scorer_1)$log_scores)
lpa <- length(ScoreTableNode(partitioned_nodes, 'A', scorer_1)$parent_combinations)
testthat::test_that('Lengths of ScoreTableNode elements match', {
  testthat::expect_equal(lscores, lpa)
})

testthat::test_that('ScoreNode works', {
  testthat::expect_true(ScoreNode(partitioned_nodes, 'A', scorer_1) < 0.0)
  testthat::expect_true(ScoreNode(partitioned_nodes, 'C', scorer_2) < 0.0)
})

bn_dag <- bnlearn::empty.graph(colnames(data))
bnlearn::amat(bn_dag) <- dag
testthat::test_that('Check ScoreDAG with BNLearn against bnlearn::score', {
  testthat::expect_equal(ScoreDAG(dag, scorer_1), bnlearn::score(bn_dag, data = data))
})

test_that('ScoreLabelledPartition are less than 0', {
  testthat::expect_true(ScoreLabelledPartition(partitioned_nodes, scorer_1) < 0.0)
  testthat::expect_true(ScoreLabelledPartition(partitioned_nodes, scorer_2) < 0.0)
})

whitelist <- matrix(FALSE, 
                    ncol = ncol(data), 
                    nrow = ncol(data), 
                    dimnames = list(colnames(data), colnames(data)))
whitelist['A', 'B'] <- TRUE
false_whitelist <- data.frame(partition = c(1, 1), node = c('A', 'B'))
true_whitelist <- data.frame(partition = c(1, 2), node = c('A', 'B'))
testthat::test_that('CheckWhitelist works', {
  testthat::expect_false(CheckWhitelistObeyed(false_whitelist, whitelist))
  testthat::expect_true(CheckWhitelistObeyed(true_whitelist, whitelist))
})

blacklist <- matrix(FALSE, 
                    ncol = ncol(data), 
                    nrow = ncol(data), 
                    dimnames = list(colnames(data), colnames(data)))
blacklist['A', 'B'] <- TRUE
true_blacklist <- data.frame(partition = c(1, 1), node = c('A', 'B'))
false_blacklist <- data.frame(partition = c(1, 2), node = c('A', 'B'))
testthat::test_that('CheckBlacklist works', {
  testthat::expect_false(CheckBlacklistObeyed(false_blacklist, blacklist))
  testthat::expect_true(CheckBlacklistObeyed(true_blacklist, blacklist))
})
