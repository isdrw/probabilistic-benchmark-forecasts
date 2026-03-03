library(arrow)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(dplyr)

#==========================================================

#data from Eval CSV
eval_df <- read.csv("data/Evaluation results/evaluation_table.CSV", sep = ";")

#===========================================================
#all WIS over horizons for selected Dataset

selected_dataset <- "Random Walk"
selected_target <- "cpi"

selected_methods <- c(
  "gauss_quantiles_prediction",
  "linear_quantile_regression",
  "QAR"
)

selected_variation <- "fitted_mean"


df_plot <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset == selected_dataset,
    target == selected_target,
    method %in% selected_methods,
    tau == 0.8,
    variation %in% c("", selected_variation)
  )

#plot

ggplot(df_plot, aes(x = horizon, y = WIS_all, color = method)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Weighted Interval Score over Horizons",
    subtitle = paste("Dataset:", selected_dataset, "| Target:", selected_target),
    x = "Forecast Horizon (h)",
    y = "WIS",
    color = "Method"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#======================================================================
#Coverage

selected_dataset <- "WEO"
selected_target <- "cpi"
selected_tau <- 0.8

selected_methods <- c(
  "gauss_quantiles_prediction",
  "empirical_quantiles_prediction",
  "bayesian_quantile_regression",
  "QAR",
  "unconditional_quantiles"
)

selected_variation <- "fitted_mean"


df_plot_2 <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset == selected_dataset,
    target == selected_target,
    method %in% selected_methods,
    tau == selected_tau,
    variation %in% c("", selected_variation)
  )

ggplot(df_plot_2, aes(x = horizon, y = coverage, color = method)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = selected_tau, 
             linetype = "solid", 
             size = 0.8,
             color = "black") +
  labs(
    title = "Weighted Interval Score over Horizons",
    subtitle = paste("Dataset:", selected_dataset, "| Target:", selected_target),
    x = "Forecast Horizon (h)",
    y = "Coverage",
    color = "Method"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
