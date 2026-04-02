library(arrow)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(dplyr)
library(forcats)
library(tidytext)

####
#Ratio for relative mWIS plots 811x612 
#Ratio for coverage plots 787x366
#Ratio for calibration plots 1030x746
#Ratio for mWIS by source and Method 971x516
####

#==========================================================

#data from Eval CSV
eval_df <- read.csv("data/Evaluation results/evaluation_table5000 02_04.CSV", sep = ";")
eval_df <- eval_df %>% filter(horizon %in% c(0.0, 0.5, 1.0, 1.5))

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

horizon_labels_q <- c(
  "0.0" = "1 Quarter ahead",
  "0.5" = "3 Quarters ahead",
  "1.0" = "5 Quarters ahead",
  "1.5" = "7 Quarters ahead"
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

horizon_order_q <- c(
  "1 Quarter ahead",
  "3 Quarters ahead",
  "5 Quarters ahead",
  "7 Quarters ahead"
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
    horizon = recode(horizon, !!!horizon_labels_q),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order_q)
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
  "empirical_quantiles_prediction",
  "gauss_quantiles_prediction",
  "t_quantiles_prediction",
  "skewed_t_quantiles_prediction",
  "ald_quantiles_prediction"
)

baseline <- "Empirical"


#====================
#absolute performance
df_plot <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% c("WEO"),
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
  group_by(horizon, method, target, dataset) %>%
  summarise(WIS_all = mean(WIS_all, na.rm = TRUE), .groups = "drop") 

#relative performance per horizon
df_rel <- df_plot %>%
  left_join(
    df_plot %>%
      filter(method == baseline) %>%
      select(horizon, target, baseline = WIS_all),
    by = c("horizon", "target")  # join by both horizon and target
  ) %>%
  mutate(rel_perf = WIS_all / baseline) %>% 
  group_by(horizon) %>%
  mutate(method_reordered = reorder_within(method, rel_perf, horizon)) %>%
  ungroup()

max_rel_perf <- min(2,max(abs(df_rel$rel_perf-1)))

# Dumbbell plot (all datasets)
ggplot(df_rel , aes(y = fct_rev(method), x = rel_perf, color = method, shape = method)) +
  
  # baseline ±5% band
  annotate("rect", xmin = 0.95, xmax = 1.05, ymin = -Inf, ymax = Inf,
           fill = "lightblue", alpha = 0.5) +

  
  # line from baseline to method point
  geom_segment(aes(y = fct_rev(method), yend = fct_rev(method), 
                   x = 1, xend = rel_perf),
               color = "gray70", linewidth = 1) +
  
  # method points
  geom_point(size = 4) +
  
  # baseline at 1
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1, color = "black") +
  
  # vertical stacking of horizons
  facet_wrap(~horizon, ncol = 1, scales = "free_y") +
  
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  
  labs(
    title = paste0("Relative mWIS (Baseline: ", baseline, ")"),
    subtitle = paste0(
      "Point Forecast Source: ", df_plot$dataset,
      " | Target:", unique(df_plot$target)
    ),
    y = NULL,
    x = "Relative mWIS",
    color = "Method",
    shape = "Method"
  ) +
  coord_cartesian(xlim = c(1 - max_rel_perf - 0.01, 1 + max_rel_perf + 0.01)) +  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Dumbbell plot (one dataset)
ggplot(df_rel, aes(y = fct_rev(method), x = rel_perf, color = method, shape = method)) +
  
  # baseline ±5% band
  annotate("rect", xmin = 0.95, xmax = 1.05, ymin = -Inf, ymax = Inf,
           fill = "lightblue", alpha = 0.5) +
  
  
  # line from baseline to method point
  geom_segment(aes(y = fct_rev(method), yend = fct_rev(method), 
                   x = 1, xend = rel_perf),
               color = "gray70", linewidth = 1) +
  
  # method points
  geom_point(size = 4) +
  
  # baseline at 1
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1, color = "black") +
  
  # vertical stacking of horizons
  facet_wrap(~horizon, ncol = 1, scales = "free_y") +
  
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  
  labs(
    title = paste0("Relative mWIS (Baseline: ", baseline, ")"),
    subtitle = paste(
      "Point Forecast Source: ARIMA BIC",
      "| Target:", unique(df_plot$target)
    ),
    y = NULL,
    x = "Relative mWIS",
    color = "Method",
    shape = "Method"
  ) +
  coord_cartesian(xlim = c(1 - max_rel_perf - 0.01, 1 + max_rel_perf + 0.01)) +  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


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

selected_methods <- c(
  "empirical_quantiles_prediction",
  "gauss_quantiles_prediction",
  "t_quantiles_prediction",
  "skewed_t_quantiles_prediction",
  "ald_quantiles_prediction"
)



#filter eval dataset 
df_plot_2 <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% "WEO",
    target %in% c("cpi","gdp"),
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
  group_by(horizon, method, target, dataset) %>%
  summarise(coverage = mean(coverage, na.rm = TRUE), .groups = "drop")
  

#plot point-lines
ggplot(df_plot_2, aes(
  x = horizon, 
  y = coverage, 
  color = method, 
  group = method, 
  shape = method)) +
  geom_line(linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 4.5) +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = selected_tau, 
             linetype = "solid", 
             linewidth = 0.8,
             color = "black") +
  labs(
    title = "Empirical Coverage over Horizons",
    subtitle = paste("Point Forecast Source:", df_plot_2$dataset),
    x = "Forecast Origin",
    y = "Coverage",
    color = "Method",
    shape = "Method"
  ) +
  facet_wrap(~ target, nrow = 1) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

#plot point-lines (one forecast source)
ggplot(df_plot_2, aes(
  x = horizon, 
  y = coverage, 
  color = method, 
  group = method, 
  shape = method)) +
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
    subtitle = paste("Point Forecast Source: AR(1)","| Target:", df_plot_2$target),
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

selected_methods <- c(
  "empirical_quantiles_prediction",
  "t_quantiles_prediction",
  "skewed_t_quantiles_prediction"
)

#data frame for calibration plot
df_plot_3 <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% "WEO",
    target == "gdp",
    method %in% selected_methods,
    variation %in% all_variations,
    horizon %in% selected_horizon
  ) %>% 
  mutate(
    method = recode(method, !!!method_labels),
    variation = recode(variation, !!!variation_labels),
    dataset = recode(dataset, !!!dataset_labels),
    horizon = recode(horizon, !!!horizon_labels_q),
    target = recode(target, !!!target_labels),
    horizon = factor(horizon, levels = horizon_order_q)
  ) %>%
  group_by(method, target, tau, horizon, dataset) %>%
  summarise(
    coverage = mean(coverage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(method, tau)

ggplot(df_plot_3, aes(x = tau, y = coverage,
                      color = method,
                      shape = method,
                      group = method)) +
  geom_line(linewidth = 0.9, linetype = "solid") +
  geom_point(size = 4) +
  
  geom_abline(slope = 1, intercept = 0, linewidth = 1, linetype = "solid") +
  
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  
  labs(
    title = "Calibration Forecast-Error-based Methods",
    subtitle = paste("Average across Horizons ", 
                     "| Point Forecast Source: ", df_plot_3$dataset, 
                     "| Target: ", df_plot_3$target),  
    x = expression(tau),
    y = "Empirical Coverage",
    color = "Method",
    shape = "Method"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#facet wrapped around horizon
ggplot(df_plot_3, aes(x = tau, y = coverage,
                      color = method,
                      shape = method,
                      group = method)) +
  geom_line(linewidth = 0.9, linetype = "solid") +
  geom_point(size = 4) +
  
  geom_abline(slope = 1, intercept = 0,
              linewidth = 1, linetype = "solid") +
  
  facet_wrap(~ horizon, ncol = 2) +   
  
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  
  labs(
    title = "Calibration Empirical vs. Normal",
    subtitle = paste0("All Point Forecast Sources (average) | ","Target: ",df_plot_3$target),
    x = expression(tau),
    y = "Empirical Coverage",
    color = "Method",
    shape = "Method"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

#facet wrapped around horizon (one source)
ggplot(df_plot_3, aes(x = tau, y = coverage,
                      color = method,
                      shape = method,
                      group = method)) +
  geom_line(linewidth = 0.9, linetype = "solid") +
  geom_point(size = 4) +
  
  geom_abline(slope = 1, intercept = 0,
              linewidth = 1, linetype = "solid") +
  
  facet_wrap(~ horizon, ncol = 2) +   # <-- 2x2 layout
  
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  
  labs(
    title = "Calibration of Empirical vs. Student-t and Skewed-t",
    subtitle = paste0("Point Forecast Source: ", df_plot_3$dataset,
                      "| Target: ",df_plot_3$target),
    x = expression(tau),
    y = "Empirical Coverage",
    color = "Method",
    shape = "Method"
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
    dataset %in% c("WEO"),
    target %in% c("cpi","gdp"),
    method %in% c("t_quantiles_prediction",
                  "skewed_t_quantiles_prediction",
                  "gauss_quantiles_prediction",
                  "ald_quantiles_prediction",
                  "empirical_quantiles_prediction"),
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
  group_by(method, target, dataset, horizon) %>%
  summarise(
    WIS_all = mean(WIS_all, na.rm = TRUE),
    mean_coverage = mean(coverage, na.rm = TRUE),
    .groups = "drop"
    ) %>% print(n = 100)

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




#===============================================
# Idea for comparison of point forecast sources
#===============================================

selected_methods <- c(
  "empirical_quantiles_prediction",
  "gauss_quantiles_prediction",
  "t_quantiles_prediction",
  "skewed_t_quantiles_prediction",
  "ald_quantiles_prediction"
)

df_plot <- eval_df %>% 
  filter(
    frequency == "annually", 
    dataset %in% all_datasets,
    target %in% "gdp",
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
  group_by(horizon, target, dataset, method) %>%
  summarise(WIS_all = mean(WIS_all, na.rm = TRUE), .groups = "drop") 

ggplot(df_plot, aes(y = method, x = WIS_all,
                    fill = dataset)) +
  
  geom_col(position = position_dodge(width = 0.7),
           width = 0.5, alpha = 0.9) +
  
  facet_wrap(~horizon, scales = "free_x") +
  
  scale_fill_brewer(palette = "Set2") +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13)
  ) +
  
  labs(
    title = "mWIS by Point Forecast Source and Method",
    subtitle = paste0("Target: ", df_plot$target),
    x = "mWIS",
    y = NULL,
    fill = "Forecast\nSource"
  )
#===============================================
#===============================================
#===============================================