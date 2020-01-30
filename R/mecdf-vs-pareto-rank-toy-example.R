library(data.table)
library(tidyverse)
library(Emcdf)
library(nsga2R)
library(cowplot)

set.seed(123)
n <- 200
x <- rnorm(n)
y <- rnorm(n)

data <- data.frame(idx = 1:(n+n/2+1), x = abs(c(0, x, rep(0, n/2) + rnorm(n = n/2, sd = 0.1))), y = abs(c(4, y, rep(0, n/2) + rnorm(n = n/2, sd = 0.1))))
data_matrix <- as.matrix(data[, c("x", "y")])

mecdf_start <- Sys.time()
data$mecdf <- Emcdf::emcdf(data = data_matrix, a = data_matrix)
mecdf_time <- as.numeric(Sys.time() - mecdf_start)

pareto_start_nsga2R <- Sys.time()
pareto_ranks_nsga2R <- nsga2R::fastNonDominatedSorting(data[, c("x", "y")])
pareto_time_nsga2R <- as.numeric(Sys.time() - pareto_start_nsga2R)

data <- 
  lapply(seq_along(pareto_ranks_nsga2R), FUN = function(x) {
    data.frame(idx = pareto_ranks_nsga2R[[x]], pareto_rank_nsga2R = x)
  }) %>% 
  bind_rows() %>% 
  dplyr::arrange(idx) %>% 
  dplyr::right_join(data, by = "idx")

mecdf_gg <- 
  ggplot(data, aes(x = x, y = y, color = mecdf)) +
  geom_point(cex = 4) +
  scale_color_viridis_c() +
  theme_minimal() +
  ggtitle(paste0("Multidimensional empirical cumulative distribution function\n", 
                 nrow(data),
                 " data points\n",
                 length(unique(data$mecdf)),
                 " unique rankings\n",
                 round(mecdf_time, 3),
                 " seconds to run\n",
                 "Slowdown factor compared to fastest ranking method: ", 
                 round(mecdf_time / min(mecdf_time, pareto_time), 2)))

mecdf_gg

pareto_gg <- 
  ggplot(data, aes(x = x, y = y, color = pareto_rank_nsga2R)) +
  geom_point(cex = 4) +
  scale_color_viridis_c() +
  theme_minimal() +
  ggtitle(paste0("Pareto rank {nsga2R}\n",
                 nrow(data),
                 " data points\n",
                 length(unique(data$pareto_rank_nsga2R)),
                 " unique rankings\n",
                 round(pareto_time, 3),
                 " seconds to run\n",
                 "Slowdown factor compared to fastest ranking method: ", 
                 round(pareto_time / min(mecdf_time, pareto_time_nsga2R), 2)))

pareto_gg
both_rankings_gg <- plot_grid(mecdf_gg, pareto_gg)
both_rankings_gg

ggsave(filename = "figures/mecdf-vs-pareto-rank-comparision.png")
