library(arrow)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(dplyr)

#==========================================================

#data from Eval CSV
eval_df <- read.csv("data/Evaluation results/evaluation_table5000.CSV", sep = ";")

#==========================================================
#Renaming for labels
method_labels <- c(
  "ald_quantiles_prediction" = "Asymmetric Laplace",
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

all_datasets <- c("WEO", "AR(1)", "ARIMA(1,1,0)", "Random Walk", "ARIMA BIC", "OECD")
all_variations <- c("" ,"fitted_mean", "fitted_mean & unbiased VAR", "mean0", "mean0 & unbiased VAR")

#===========================================================

#dataframe for evaluation after comparison between variations. 
#Focus on fitted mean (& unbiased VAR if available)
eval_df <- eval_df %>%
  filter(
    variation != "mean0", 
    variation != "mean0 & unbiased VAR",
    !(method == "gauss_quantiles_prediction" & variation == "fitted_mean"),
  )
#===========================================================
#all WIS over horizons for selected Dataset

selected_dataset <- c("WEO")
selected_target <- "gdp"

selected_methods <- c(
  "empirical_quantiles_prediction",
  "gauss_quantiles_prediction",
  "t_quantiles_prediction",
  "skewed_t_quantiles_prediction",
  "ald_quantiles_prediction"
)

selected_variation <- c("" ,"fitted_mean", "fitted_mean & unbiased VAR", "mean0", "mean0 & unbiased VAR")


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


#plot point-lines
ggplot(df_plot, aes(x = horizon, y = WIS_all, color = method, group = method)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("Point Forecast Source:", df_plot$dataset," | Target:", df_plot$target),
    x = "Forecast Origin",
    y = "mWIS",
    color = "Method"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#===========================================================
#average mWIS over all datasets for each horizon

selected_methods <- c(
  "linear_quantile_regression",
  "gauss_quantiles_prediction",
  "t_quantiles_prediction",
  "skewed_t_quantiles_prediction",
  "ald_quantiles_prediction",
  "empirical_quantiles_prediction",
  "bayesian_quantile_regression"
)


selected_variation <- c("" ,"fitted_mean", "fitted_mean & unbiased VAR", "mean0", "mean0 & unbiased VAR")

#====================
#absolute performance
df_plot <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% all_datasets,
    target %in% "cpi",
    method %in% selected_methods,
    tau == 0.8,
    variation %in% all_variations
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


ggplot(df_plot, aes(x = method, y = WIS_all, color = horizon, group = horizon)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "WIS Comparison Across Methods by Horizon",
    x = "Method",
    y = "WIS",
    color = "Horizon"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#===============================================
# Idea for comparison of point forecast sources
#===============================================
ggplot(df_plot, aes(y = method)) +
  geom_point(aes(x = WIS_all, color = dataset), alpha = 0.7, size = 2.5) +
  facet_wrap(~horizon, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(x = "WIS", y = NULL, color = "Forecast\nSource")
#===============================================
#===============================================
#===============================================


#====================
#plot point-lines
ggplot(df_plot, aes(x = horizon, y = WIS_all, 
                    color = method, 
                    shape = method,
                    group = method)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("All Point Forecast Sources (average)| Target:", df_plot$target),
    x = "Forecast Origin",
    y = "mWIS",
    color = "Method",
    shape = "Method"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

#plot point-lines (one Forecast Source)
ggplot(df_plot, aes(x = horizon, y = WIS_all, 
                    color = method, 
                    shape = method,
                    group = method)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("Point Forecast Source: AR(1)", "| Target:", df_plot$target),
    x = "Forecast Origin",
    y = "mWIS",
    color = "Method",
    shape = "Method"
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
    x = "Forecast Origin",
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
selected_tau <- 0.5

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
selected_target <- c("cpi","gdp")
selected_tau <- 0.8
selected_frequency <- "annually"

selected_methods <- c(
  "empirical_quantiles_prediction",
  "linear_quantile_regression",
  "bayesian_quantile_regression",
  "EasyUQ_idr",
  "QAR"
)

selected_variation <- c("", "fitted_mean", "fitted_mean & unbiased VAR", "mean0", "mean0 & unbiased VAR")

#filter eval dataset 
df_plot_2 <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% selected_dataset,
    target == selected_target,
    method %in% c("t_quantiles_prediction"),
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
  group_by(horizon, variation, target) %>%
  summarise(coverage = mean(coverage, na.rm = TRUE), .groups = "drop")
  

#plot point-lines
ggplot(df_plot_2, aes(
  x = horizon, 
  y = coverage, 
  color = variation, 
  group = variation, 
  shape = variation)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
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
    color = "Specification",
    shape = "Specification"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

#plot point-lines (one forecast source)
ggplot(df_plot_2, aes(
  x = horizon, 
  y = coverage, 
  color = variation, 
  group = variation, 
  shape = variation)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = selected_tau, 
             linetype = "solid", 
             size = 0.8,
             color = "black") +
  labs(
    title = "Empirical Coverage over Horizons",
    subtitle = paste("Point Forecast Source: WEO","| Target:", df_plot_2$target),
    x = "Forecast Origin",
    y = "Coverage",
    color = "Specification",
    shape = "Specification"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#===========================================================
#average mWIS for all datasets for each horizon and one method
selected_dataset <- c("WEO", "AR(1)", "ARIMA(1,1,0)", "Random Walk", "ARIMA BIC")
selected_target <- "gdp"
selected_methods <- c("empirical_quantiles_prediction",
                      "linear_quantile_regression",
                      "easyUQ_idr",
                      "bayesian_quantile_regression"
                      )
selected_frequency <- "annually"


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
  group_by(horizon, dataset, target) %>%
  summarise(WIS_all = mean(WIS_all, na.rm = TRUE), .groups = "drop")

#plot lines
ggplot(df_plot, aes(x = horizon, y = WIS_all, color = dataset, group = dataset)) +
  geom_line(size = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  
  scale_color_brewer(palette = "Set2") +
  
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("Target:", unique(df_plot$target),
                     "| All Methods (average)"),
    x = "Forecast Origin",
    y = "mWIS",
    color = "Source"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

#plot bars

ggplot(df_plot, aes(x = factor(horizon), y = WIS_all, fill = dataset)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("Target:", df_plot$target,
                     "| all Methods (average):"),
    x = "Forecast Origin",
    y = "mWIS",
    fill = "Source"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )




#multi method plot
ggplot(df_plot, aes(x = horizon, y = WIS_all, fill = dataset)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  
  facet_wrap(~method, nrow = 1) +
  
  labs(
    title = "Mean Weighted Interval Score",
    subtitle = paste("Target:", unique(df_plot$target)),
    x = "Forecast Origin",
    y = "mWIS",
    fill = "Dataset"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#========================================================================================
# Calibration Plots
#========================================================================================
selected_horizon <- c(0.0, 0.5, 1.0, 1.5)

#data frame for calibration plot
df_plot_3 <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% selected_dataset,
    target == "gdp",
    method %in% selected_methods,
    variation %in% selected_variation,
    horizon %in% selected_horizon
  ) %>% 
  mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels),
    target = recode(target, !!!target_labels)
  ) %>%
  group_by(variation, target, tau) %>%
  summarise(
    coverage = mean(coverage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(variation, tau)

ggplot(df_plot_3, aes(x = tau, y = coverage,
                      color = variation,
                      shape = variation,
                      group = variation)) +
  geom_line(linewidth = 0.9, linetype = "solid") +
  geom_point(size = 4) +
  
  geom_abline(slope = 1, intercept = 0, linewidth = 1, linetype = "dashed") +
  
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  
  labs(
    title = "Calibration",
    subtitle = paste("Horizon:", "| Target: ", df_plot_3$target),  
    x = expression(tau),
    y = "Empirical Coverage",
    color = "Specification",
    shape = "Specification"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )



#================================================
#mWIS average of specs across horizons for tables
eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% all_datasets,
    target %in% c("cpi"),
    method %in% selected_methods,
    tau == 0.8,
    variation %in% all_variations
  ) %>% mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order)
  ) %>%
  group_by(method, target, horizon) %>%
  summarise(WIS_all = mean(WIS_all, na.rm = TRUE), .groups = "drop")

#================================================
#coverage average of specs across horizons for tables
eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% all_datasets,
    target %in% c("cpi"),
    method %in% c("t_quantiles_prediction"),
    tau == 0.8,
    variation %in% all_variations
  ) %>% mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order)
  ) %>%
  group_by(variation, target, dataset) %>%
  summarise(WIS_all = mean(coverage, na.rm = TRUE), .groups = "drop")
