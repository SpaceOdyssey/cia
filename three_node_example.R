## ---- include = FALSE--------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup-------------------------------------------------------------------------------------------------------------------------
library(dagmc)
library(magrittr)


## ---- error = TRUE, fig.dim = c(6.0, 6.0)------------------------------------------------------------------------------------------
nodes <- LETTERS[1:3]
# set.seed(2)
# true_dag <- UniformlySampleDAG(nodes)
# 
# true_graph <- bnlearn::empty.graph(nodes)
# bnlearn::amat(true_graph) <- true_dag
# plot(true_graph)


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
levels <- c('yes', 'no')
all_levels <- list(A = levels, B = levels, C = levels)

# Single edge model
bn_arr <- list()
bn_arr[[1]] <- gRbase::parray(c('A'), levels = all_levels,
                              values = c(0.5, 0.5))
bn_arr[[2]] <- gRbase::parray('B', levels = all_levels,
                              values = c(0.5, 0.5))
bn_arr[[3]] <- gRbase::parray(c('C', 'B'), levels = all_levels,
                              values = c(0.1, 0.9, 0.9, 0.1))

cpt <- gRain::compileCPT(bn_arr)
gr_dag <- gRain::grain(cpt)

true_dag <- as(gr_dag$dag, 'matrix')

# # Two-edge model
# bn_arr <- list()
# bn_arr[[1]] <- gRbase::parray(c('A', 'B'), levels = all_levels,
#                               values = c(0.9, 0.1, 0.1, 0.9))
# bn_arr[[2]] <- gRbase::parray('B', levels = all_levels,
#                               values = c(0.5, 0.5))
# bn_arr[[3]] <- gRbase::parray(c('C', 'B'), levels = all_levels,
#                               values = c(0.1, 0.9, 0.9, 0.1))
# 
# cpt <- gRain::compileCPT(bn_arr)
# gr_dag <- gRain::grain(cpt)
# 
# true_dag <- as(gr_dag$dag, 'matrix')

plot(gr_dag$dag)


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
data <- stats::simulate(gr_dag, nsim = 500, seed = 1)
knitr::kable(head(data))


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
knitr::kable(cor(1*(data == 'yes')))


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
GetAllDAGs <- function(nodes) {
  tri_val <- expand.grid(c(0, 1), c(0, 1), c(0, 1))
  node_perm <- gtools::permutations(length(nodes), length(nodes), nodes)
  all_dags <- list()
  n <- 1
  for (i in 1:nrow(node_perm)) {
    for (j in 1:nrow(tri_val)) {
      mat <- matrix(
        0L,
        nrow = length(nodes), 
        ncol = length(nodes),
        dimnames = list(node_perm[i, ], node_perm[i, ])
        )
      mat[upper.tri(mat)] <- as.numeric(tri_val[j, ])
      mat <- mat[nodes, nodes]
      
      all_dags[[n]] <- mat
      n <- n + 1
    }
  }
  all_dags <- unique(all_dags)
  
  return(all_dags)
}

all_dags <- GetAllDAGs(nodes)


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
scorer <- list(scorer = memo::memo(BNLearnScorer), parameters = list(data = data, type = 'bde'))
score_all_dags <- unlist(lapply(all_dags, ScoreDAG, scorer = scorer))
hash_all_dags <- all_dags %>%
  lapply(rlang::hash) %>% 
  unlist


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
log_z <- LogSumExp(score_all_dags)
log_p <- score_all_dags - log_z
p_true <- exp(log_p)

p_summary <- data.frame(
  hash_dag = hash_all_dags, 
  p_true = p_true
)


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
set.seed(1)
n_results <- 10000
n_cores <- parallel::detectCores()

init_partitions <- list()
for (i in 1:n_cores) {
  init_dag <- UniformlySampleDAG(names(data))
  init_partitions[[i]] <- GetPartitionedNodesFromAdjacencyMatrix(init_dag)
}

system.time(
  chains <- SampleChains(n_results, init_partitions, 
                         transition = PartitionMCMC(),
                         scorer = scorer,
                         n_parallel_chains = n_cores)
)


## ---- error = TRUE, fig.dim = c(6.0, 4.0)------------------------------------------------------------------------------------------
PlotScoreTrace <- function(chains, burnin = 1, nrow = NULL, ncol = 1, ...) {
  
  colors <- c('black', 'green', 'red', 'blue', 'brown', 'darkviolet', 
              'darkgoldenrod', 'deeppink')
  
  n_within <- length(chains[[1]]$log_score)
  plot(chains[[1]]$log_score[burnin:n_within], col = colors[1], ...)
  
  for (i in 2:length(chains)) {
    n_within <- length(chains[[i]]$log_score)
    lines(chains[[i]]$log_score[burnin:n_within], 
          col = colors[i %% length(colors)], ...)
  }
}

PlotScoreTrace(chains, ylab = 'log(partition score)', type = 'l')


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
n_burnin <- 100
set.seed(1)
dags <- list()
score_dags <- list()
for (i in 1:n_cores) {
  eq_states <- chains[[i]]$state[(1 + n_burnin):n_results]
  dags[[i]] <- lapply(eq_states, SampleDAGFromLabelledPartition, scorer = scorer)
  score_dags[[i]] <- unlist(lapply(dags[[i]], ScoreDAG, scorer = scorer))
}


## ---- error = TRUE, fig.dim = c(6.0, 4.0)------------------------------------------------------------------------------------------
true_score <- ScoreDAG(true_dag, scorer)
n_target_saves <- n_results - n_burnin

colors <- c('black', 'green', 'red', 'blue', 'brown', 'darkviolet', 
            'darkgoldenrod', 'deeppink')
plot(1:n_target_saves, score_dags[[1]], xlab = 'saves', ylab = 'log(DAG score)', type = 'l')
for (i in 2:n_cores) {
  lines(score_dags[[i]], xlab = 'saves', ylab = 'log(partition score)', type = 'l', col = colors[i - 1])
}


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
unique_sampled_dags <- unique(unlist(dags, recursive = FALSE))
n_within <- (n_results - n_burnin)/2

p_chains <- list()
ssq <- list()
for (i in 1:n_cores) {
  for (m in 1:2) {
    count_dags <- dags[[i]][(1 + (m - 1)*n_within):(m*n_within)] %>%
      lapply(rlang::hash) %>%
      unlist %>%
      table
    sample_p_dags <- count_dags / sum(count_dags)
    p_chains[[2*(i - 1) + m]] <- data.frame(
      chain = i,
      sequence = m,
      hash_dag = names(sample_p_dags),
      p = as.numeric(sample_p_dags)
    )
  }
}
p_chains <- dplyr::bind_rows(p_chains)

# Only going to check convergence for DAGs that appear in all chains. I'm
# assuming that other DAGs have a reasonably low probability, and therefore
# we are less interested in them.
focus_dags <- names(table(p_chains$hash_dag))[table(p_chains$hash_dag) == 2*n_cores]
p_chains <- p_chains %>%
  dplyr::filter(hash_dag %in% focus_dags)

p_marg <- p_chains %>%
  dplyr::group_by(hash_dag) %>%
  dplyr::summarise(p = mean(p))

psi_j <- p_chains %>%
  dplyr::group_by(chain, hash_dag) %>%
  dplyr::summarise(psi_j = mean(p), ssq_j = p*(1.0 - p))

psi_bar <- psi_j %>%
  dplyr::group_by(hash_dag) %>%
  dplyr::summarise(psi_bar = mean(psi_j))

psi <- dplyr::left_join(psi_j, psi_bar, by = 'hash_dag')

bw <- psi %>%
  dplyr::mutate(psi_diff_sq = (psi_j - psi_bar)^2) %>%
  dplyr::group_by(hash_dag) %>%
  dplyr::summarise(
    b = n_within/(2*n_cores - 1)*sum(psi_diff_sq),
    w = mean(ssq_j)
    ) %>%
  dplyr::mutate(
    var = (w*(n_within - 1) + b)/n_within,
    rhat = sqrt(var/w)
    ) %>%
  dplyr::left_join(p_marg, by = 'hash_dag') %>%
  dplyr::left_join(p_summary, by = 'hash_dag') %>%
  dplyr::arrange(desc(p_true))


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
t_max <- 100
n_eff <- c()
for (i_focus_dag in 1:1) {
  v <- c()
  rho <- c()
  for (t in 0:(t_max - 1)) {
    sum_psi_sq <- 0.0
    for (r in 1:n_cores) {
      for (m in 1:2) {
        dags_j <- dags[[r]][(1 + (m - 1)*n_within):(m*n_within)] %>%
              lapply(rlang::hash) %>%
              unlist
        psi_ij <- dags_j == bw$hash_dag[i_focus_dag]
        
        sum_psi_sq_j <- sum((psi_ij[(t + 1):n_within] - psi_ij[1:(n_within - t)])^2)
        sum_psi_sq <- sum_psi_sq + sum_psi_sq_j
      }
    }
    v_t_const <- 1.0/(2*n_cores*(n_within - t))
    v_t <- v_t_const*sum_psi_sq
    v <- c(v, v_t)
  
    rho_t <- 1.0 - 0.5*v_t/bw$var[i_focus_dag]
    rho <- c(rho, rho_t)
  }
  
  # Only perform a partial sum of rho
  rho_pair_sum <- rho[seq(0, t_max, 2)] + rho[seq(1, t_max, 2)]
  if (all(rho_pair_sum > 0)) {
    # full sum
    rho_part_sum <- sum(rho)
  } else {
    # partial sum
    first_neg_pair <- which(rho_pair_sum < 0)[1]
    rho_part_sum <- sum(rho[1:(first_neg_pair - 1)])
  }
  
  n_eff_dag <- 2*n_cores*n_within/(1 + 2*rho_part_sum)
  n_eff <- c(n_eff, n_eff_dag)
}
bw1 <- bw %>% 
  dplyr::bind_cols(n_eff = n_eff) %>%
  dplyr::mutate(
    p_sd = sqrt(p*(1.0 - p)/n_eff),
    p_within_sd = (p >= p_true - p_sd) & (p <= p_true + p_sd)
    )

knitr::kable(bw1)


## ---- error = TRUE-----------------------------------------------------------------------------------------------------------------
chain_info <- dplyr::bind_cols(
  proposal_used = unlist(chains[[1]]$proposal_info[n_burnin:n_results], use.names = FALSE), 
  accept = unlist(chains[[1]]$mcmc_info[n_burnin:n_results], use.names = FALSE)
  )

prop_accept <- chain_info %>% 
  dplyr::group_by(proposal_used) %>% 
  dplyr::summarise(accept = mean(accept))

knitr::kable(prop_accept)


## ---- error = TRUE, fig.dim = c(7.0, 12.0)-----------------------------------------------------------------------------------------
n_chain_all <- dags %>%
  unlist(recursive = FALSE) %>%
  lapply(rlang::hash) %>%
  unlist %>%
  table
p_chain_all <- n_chain_all/sum(n_chain_all)

i_order <- order(p_true, decreasing = TRUE)
graphics::par(mfrow = c(4, 2))
for (i in i_order) {
  hash_focus_dag <- hash_all_dags[i]
  focus_dag <- all_dags[[i]]
  
  bn_focus <- bnlearn::empty.graph(nodes)
  bnlearn::amat(bn_focus) <- focus_dag
  plot(bn_focus)
  
  t <- paste('p_est: ', round(p_chain_all[hash_focus_dag], 4), 
             ', p_true: ', round(p_true[i], 4), sep = '')
  graphics::text(300, 0, t)
}
graphics::par(mfrow = c(1,1))

