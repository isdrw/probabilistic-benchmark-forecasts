
library(arrow)
library(tidyverse)
library(ggplot2)
library(patchwork)

#==========================================================

#data from Eval CSV
eval_df <- read.csv("data/Evaluation results/evaluation_table.CSV", sep = ";")

#=========================================================

#WIS mean summary over horizon dataset target combination
#and average relative absolute deviation for dataset target combination

WIS_eval <- eval_df %>%
  filter(method == "gauss_quantiles_prediction", tau == 0.8, frequency == "annually") %>%
  group_by(dataset, horizon, target) %>% 
  mutate(
    mean_WIS = mean(WIS_all),
    WIS_rel_MAD = mean(abs(WIS_all - mean_WIS)) / mean_WIS,
    WIS_max_rel_dev = max(abs(WIS_all - mean_WIS)) / mean_WIS
  ) %>% 
  ungroup() %>%
  group_by(dataset, target) %>%
  mutate(
    WIS_rel_dispersion_target = mean(WIS_rel_MAD)
  ) %>% 
  ungroup()

WIS_eval %>% 
  group_by(dataset, target) %>%
  summarise(
    avg_rel_abs_dev = first(WIS_rel_dispersion_target),
    .groups = "drop"
  )

#finding best method
WIS_best <- eval_df %>% 
  group_by(target, dataset, method) %>% 
  mutate(
    WIS_sum = sum(WIS_all)
  ) %>%
  ungroup() %>%
  group_by(target, dataset) %>%
  mutate(
    method_best = method[which.min(WIS_sum)]
  ) %>% 
  ungroup()

WIS_best %>%
  group_by(target, dataset) %>%
  summarise(
    method_best = first(method_best),
    .groups = "drop"
  )
#order for horizons
data_all$horizon <- factor(
  data_all$horizon,
  levels = c("0.0", "0.5",
             "1.0", "1.5")
)

#=============================================================================
#absolute WIS for selected dataset over all horizons and methods

selected_dataset <- "WEO"

data_filtered <- eval_df %>%
  filter(dataset == selected_dataset) %>%
  mutate(
    horizon = factor(horizon, levels = c(0.0, 0.5, 1.0, 1.5)),
    # Recode to nicer x-axis labels
    horizon = fct_recode(horizon,
                         "Fall, current"   = "0.0",
                         "Spring, current" = "0.5",
                         "Fall, next"      = "1.0",
                         "Spring, next"    = "1.5"),
    target = factor(target, levels = c("cpi", "gdp")),
    method = factor(method)
  ) %>%
  droplevels()

# Compute mean WIS per horizon (optional horizontal lines)
horizon_means <- data_filtered %>%
  group_by(target, horizon) %>%
  summarise(mean_WIS = mean(WIS, na.rm = TRUE), .groups = "drop")

# Plot with lines and points
ggplot(data_filtered,
       aes(x = horizon, y = WIS, color = method, shape = method, group = method, linetype = method)) +
  
  geom_line(size = 1.25) +     # lines connecting points
  geom_point(size = 3) +    # points at each horizon
  
  facet_wrap(~Target, nrow = 1, scales = "fixed") +  # separate plot per Target
  
  labs(
    x = "Horizon",
    y = "WIS",
    title = paste("WIS for ", selected_dataset, " Dataset"),
    color = "Method",
    shape = "Method",
    linetype = "Method"
  ) +
  
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +  # distinct shapes per method
  scale_linetype_manual(values = c("dashed", "dotted", "dotted", "dotdash")) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

#===================================================================
#relative WIS of selected dataset over all horizons and methods

selected_dataset <- "WEO"

data_rel <- data_all %>%
  filter(dataset == selected_dataset) %>%
  mutate(
    horizon = factor(horizon, levels = c(0.0, 0.5, 1.0, 1.5)),
    # Recode to nicer x-axis labels
    horizon = fct_recode(horizon,
                         "Fall, current"   = "0.0",
                         "Spring, current" = "0.5",
                         "Fall, next"      = "1.0",
                         "Spring, next"    = "1.5"),
    target = factor(target, levels = c("cpi", "gdp")),
    method = factor(method)
  ) %>%
  group_by(target, horizon) %>%
  mutate(rel_WIS = WIS / mean(WIS, na.rm = TRUE)) %>%
  ungroup() %>%
  droplevels()


#average WIS over all gauss variations (annual) all datasets and all horizons
eval_df %>% 
  filter(method == "gauss_quantiles_prediction", 
         frequency == "annually", 
         tau %in% c(0.5, 0.8),
         dataset != "AR(1)")  %>%
  group_by(variation, target) %>%
  summarise(
    mean_WIS = mean(WIS_all),
    .groups = "drop"
  )



#Plot
ggplot(data_rel,
       aes(x = Horizon, y = rel_WIS, color = Method, shape = Method,
           group = Method, linetype = Method)) +
  
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  
  facet_wrap(~Target, nrow = 1, scales = "fixed") +
  
  labs(
    x = "Horizon",
    y = "Relative WIS",
    title = paste("Relative WIS for", selected_dataset, "Dataset"),
    color = "Method",
    shape = "Method",
    linetype = "Method"
  ) +
  
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted", "dotdash")) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

#=================================================================
#WIS of all methods and all datasets for one horizon


selected_horizon <- "Spring, next"

desired_levels <- c("WEO", "Random Walk", "AR(1)", "ARIMA(1,1,0)", "ARIMA BIC")

wis_h1 <- eval_df %>%
  filter(method == "gauss_quantiles_prediction") %>%
  mutate(
    horizon = factor(horizon,
                     levels = c(0.0, 0.5, 1.0, 1.5),
                     labels = c("Fall, current",
                                "Spring, current",
                                "Fall, next",
                                "Spring, next")),
    target = factor(target, levels = c("cpi", "gdp")),
    method = factor(method),
    dataset = fct_relevel(dataset, desired_levels)
  ) %>%
  filter(horizon == selected_horizon) %>%
  droplevels()



ggplot(
  wis_h1,
  aes(x = dataset, y = WIS_all, fill = variation)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "white"
  ) +
  facet_wrap(~ target, nrow = 1, scales = "fixed") +
  labs(
    x = "Dataset",
    y = "WIS",
    title = paste0("WIS by Dataset (Horizon = ", selected_horizon, ")"),
    fill = "Method"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1.2, "lines")
  )



#==========================================================
#coverage analysis
#==========================================================

eval_df %>% 
  filter(method == "gauss_quantiles_prediction", frequency == "annually", tau %in% c(0.5, 0.8))  %>%
  group_by(variation, target, tau) %>%
  summarise(
    mean_cov = mean(coverage),
    .groups = "drop"
  )

#==========================================================
#coverage plots
#==========================================================


#select nominal coverage and dataset
selected_tau <- 0.8
selected_dataset <- "ARIMA_BIC"

coverage_weo_plot <- coverage_all %>%
  filter(Dataset == selected_dataset) %>%
  mutate(
    Horizon = factor(Horizon, levels = c("Fall_current",
                                         "Spring_current",
                                         "Fall_next",
                                         "Spring_next")),
    Target = factor(Target, levels = c("CPI", "GDP")),
    Method = factor(Method, levels = c("Mean0_VARml",
                                       "MeanEst_VARml",
                                       "Mean0_VARunb",
                                       "MeanEst_VARunb")),
    Alpha = factor(as.character(Alpha))
  ) %>%
  droplevels()



coverage_weo_plot_sub <- subset(coverage_weo_plot, Alpha == selected_tau)

ggplot(coverage_weo_plot_sub,
       aes(x = Horizon,
           y = Coverage,
           color = Method,
           linetype = Method,
           shape = Method,
           group = Method)) +
  
  geom_hline(yintercept = selected_tau, linetype = "dashed", color = "black") +
  
  geom_line(size = 1) +
  geom_point(size = 3) +
  
  facet_wrap(~Target, nrow = 1, scales = "fixed") +
  
  # → SAME SCHEME AS BEFORE
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted", "dotdash")) +
  
  labs(
    x = "Forecast Horizon",
    y = "Empirical Coverage",
    title = paste0("Empirical Coverage for ", selected_dataset, " Dataset (τ = ", selected_tau,")"),
    color = "Method",
    shape = "Method",
    linetype = "Method"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1.2, "lines")
  )

#===========================================
#coverage against all datasets on one horizon
#============================================


selected_horizon <- "Spring_current"
selected_tau <- 0.8

coverage_summary <- coverage_all %>%
  filter(Horizon == selected_horizon) %>%   
  group_by(Dataset, Target, Method, Alpha) %>%
  summarise(
    Coverage = mean(Coverage),
    .groups = "drop"
  )

coverage_summary <- coverage_summary %>%
  mutate(
    Mean = ifelse(grepl("Mean0", Method), "Mean = 0", "Mean estimated"),
    Variance = ifelse(grepl("VARunb", Method), "Unbiased variance", "ML variance")
  )

coverage_single <- coverage_summary %>%
  filter(Alpha == selected_tau)

ggplot(coverage_single,
       aes(x = Dataset,
           y = Coverage,
           fill = Method)) +
  
  geom_col(position = position_dodge(width = 0.75),
           width = 0.7) +
  
  geom_hline(yintercept = selected_tau,
             linetype = "dashed",
             color = "black",
             size = 0.8) +
  
  facet_wrap(~Target, nrow = 1, scales = "fixed") +
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    x = "Dataset",
    y = paste0("Empirical Coverage"),
    title = paste0("Empirical Coverage Across Datasets (",
                   selected_horizon,
                   ", τ = ", selected_tau, ")"),
    fill = "Method"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1.2, "lines")
  ) + 
  coord_cartesian(ylim = c(min(coverage_single$Coverage), selected_tau + 0.15))

#=================================================================
#
#=================================================================



 
