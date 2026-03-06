library(arrow)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(dplyr)

#==========================================================

#data from Eval CSV
eval_df <- read.csv("data/Evaluation results/evaluation_table.CSV", sep = ";")

#==========================================================
#Renaming for labels
method_labels <- c(
  "ald_quantiles_prediction" = "ALD",
  "gauss_quantiles_prediction" = "Normal",
  "t_quantiles_prediction" = "Student-t",
  "skewed_t_quantiles_prediction" = "Skewed t",
  "bayesian_quantile_regression" = "Bayesian QR",
  "linear_quantile_regression" = "Linear QR",
  "EasyUQ_idr" = "EasyUQ (IDR)",
  "empirical_quantiles_prediction" = "Empirical"
)

variation_labels <- c(
  "fitted_mean" = "Fitted mean",
  "fitted_mean & unbiased VAR" = "Fitted mean, unbiased VAR",
  "mean0" = "Zero mean",
  "mean0 & unbiased VAR" = "Zero mean, unbiased VAR"
)

dataset_labels <- c(
  "WEO" = "WEO",
  "AR(1)" = "AR(1)",
  "ARIMA(1,1,0)" = "ARIMA(1,1,0)",
  "Random Walk" = "Random Walk",
  "ARIMA BIC" = "ARIMA (BIC)"
)

horizon_labels <- c(
  "0.0" = "Fall, current",
  "0.5" = "Spring, current",
  "1.0" = "Fall, next",
  "1.5" = "Spring, next"
)

target_labels <- c(
  "gdp" = "GDP",
  "cpi" = "CPI"
)

horizon_order <- c(
  "Fall, current",
  "Spring, current",
  "Fall, next",
  "Spring, next"
)
#===========================================================
#all WIS over horizons for selected Dataset

selected_dataset <- c("AR(1)")
selected_target <- "cpi"

selected_methods <- c(
  "gauss_quantiles_prediction"
)

selected_variation <- c("" ,"fitted_mean", "mean0")


df_plot <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% selected_dataset,
    target == selected_target,
    method %in% selected_methods,
    tau == 0.8,
    variation %in% selected_variation
  ) %>%
  mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order)
  )


#plot

ggplot(df_plot, aes(x = horizon, y = WIS_all, color = variation, group = variation)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("Point Forecast Source:", selected_dataset, 
                     "| Target:", df_plot$target, "| Method: Normal Quantiles"),
    x = "Forecast Origin",
    y = "mWIS",
    color = "Variation"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#===========================================================
#average mWIS over all datasets for each horizon

selected_dataset <- c("WEO", "AR(1)", "ARIMA(1,1,0)", "Random Walk", "ARIMA BIC", "OECD")
selected_target <- "gdp"
selected_frequency <- "quarterly"

selected_methods <- c(
  "empirical_quantiles_prediction",
  "EasyUQ_idr",
  "linear_quantile_regression",
  "bayesian_quantile_regression",
  "QAR"
)

selected_variation <- c("" ,"fitted_mean", "fitted_mean & unbiased VAR", "mean0", "mean0 & unbiased VAR")


df_plot <- eval_df %>% 
  filter(
    frequency == selected_frequency, 
    dataset %in% selected_dataset,
    target == selected_target,
    method %in% selected_methods,
    tau == 0.8,
    variation %in% selected_variation
  ) %>% mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order)
  ) %>%
  group_by(horizon, method, target) %>%
  summarise(WIS_all = mean(WIS_all, na.rm = TRUE), .groups = "drop")
  

#plot point-lines

ggplot(df_plot, aes(x = horizon, y = WIS_all, color = method, group = method)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("All Point Forecast Sources (average)| Target:", df_plot$target),
    x = "Forecast Origin",
    y = "mWIS",
    color = "Variation"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

#plot bars

ggplot(df_plot, aes(x = factor(horizon), y = WIS_all, fill = method)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("All Point Forecast Sources (average) | Target:", df_plot$target),
    x = "Forecast Horizon",
    y = "mWIS",
    fill = "Method"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#======================================================================
#Coverage over horizons for selected Dataset

selected_dataset <- "Random Walk"
selected_target <- "cpi"
selected_tau <- 0.8

selected_methods <- c(
  "gauss_quantiles_prediction"
)

selected_variation <- c("fitted_mean", "fitted_mean & unbiased VAR", "mean0", "mean0 & unbiased VAR")


df_plot_2 <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset == selected_dataset,
    target == selected_target,
    method %in% selected_methods,
    tau == selected_tau,
    variation %in% c("", selected_variation)
  )%>%
  mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order)
  )

ggplot(df_plot_2, aes(x = horizon, y = coverage, color = variation)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = selected_tau, 
             linetype = "solid", 
             size = 0.8,
             color = "black") +
  labs(
    title = "Coverage over Horizons",
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

#======================================================================
#Coverage over all datasets for each horizon

selected_dataset <- c("WEO", "AR(1)", "ARIMA(1,1,0)", "Random Walk", "ARIMA BIC", "OECD")
selected_target <- "gdp"
selected_tau <- 0.8
selected_frequency <- "quarterly"

selected_methods <- c(
  "empirical_quantiles_prediction",
  "EasyUQ_idr",
  "linear_quantile_regression",
  "bayesian_quantile_regression",
  "QAR"
)

selected_variation <- c("", "fitted_mean", "fitted_mean & unbiased VAR", "mean0", "mean0 & unbiased VAR")

#filter eval dataset 
df_plot_2 <- eval_df %>% 
  filter(
    frequency == selected_frequency, 
    dataset %in% selected_dataset,
    target == selected_target,
    method %in% selected_methods,
    tau == selected_tau,
    variation %in% selected_variation
  ) %>% mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order)
  ) %>%
  group_by(horizon, method, target) %>%
  summarise(coverage = mean(coverage, na.rm = TRUE), .groups = "drop")
  

#plot point-lines
ggplot(df_plot_2, aes(x = horizon, y = coverage, color = method, group = method)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = selected_tau, 
             linetype = "solid", 
             size = 0.8,
             color = "black") +
  labs(
    title = "Empirical Coverage over Horizons",
    subtitle = paste("All Point Forecast Sources (average)| Target:", df_plot_2$target),
    x = "Forecast Origin",
    y = "Coverage",
    color = "Method"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

