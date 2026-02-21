
library(tidyverse)
library(ggplot2)
library(patchwork)

#==========================================================
#enter data manually
#WEO CPI data
weo_cpi <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  "CPI", "WEO", "Mean0_VARml", "Fall_current", 0.126,
  "CPI", "WEO", "Mean0_VARml", "Spring_current", 0.279,
  "CPI", "WEO", "Mean0_VARml", "Fall_next", 0.506,
  "CPI", "WEO", "Mean0_VARml", "Spring_next", 0.552,
  
  "CPI", "WEO", "MeanEst_VARml", "Fall_current", 0.133,
  "CPI", "WEO", "MeanEst_VARml", "Spring_current", 0.273,
  "CPI", "WEO", "MeanEst_VARml", "Fall_next", 0.511,
  "CPI", "WEO", "MeanEst_VARml", "Spring_next", 0.566,
  
  "CPI", "WEO", "Mean0_VARunb", "Fall_current", 0.126,
  "CPI", "WEO", "Mean0_VARunb", "Spring_current", 0.279,
  "CPI", "WEO", "Mean0_VARunb", "Fall_next", 0.504,
  "CPI", "WEO", "Mean0_VARunb", "Spring_next", 0.550,
  
  "CPI", "WEO", "MeanEst_VARunb", "Fall_current", 0.133,
  "CPI", "WEO", "MeanEst_VARunb", "Spring_current", 0.272,
  "CPI", "WEO", "MeanEst_VARunb", "Fall_next", 0.509,
  "CPI", "WEO", "MeanEst_VARunb", "Spring_next", 0.564
)

#WEO GDP data
weo_gdp <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  "GDP", "WEO", "Mean0_VARml", "Fall_current", 0.244,
  "GDP", "WEO", "Mean0_VARml", "Spring_current", 0.439,
  "GDP", "WEO", "Mean0_VARml", "Fall_next", 0.883,
  "GDP", "WEO", "Mean0_VARml", "Spring_next", 1.151,
  
  "GDP", "WEO", "MeanEst_VARml", "Fall_current", 0.260,
  "GDP", "WEO", "MeanEst_VARml", "Spring_current", 0.442,
  "GDP", "WEO", "MeanEst_VARml", "Fall_next", 0.895,
  "GDP", "WEO", "MeanEst_VARml", "Spring_next", 1.127,
  
  "GDP", "WEO", "Mean0_VARunb", "Fall_current", 0.245,
  "GDP", "WEO", "Mean0_VARunb", "Spring_current", 0.440,
  "GDP", "WEO", "Mean0_VARunb", "Fall_next", 0.883,
  "GDP", "WEO", "Mean0_VARunb", "Spring_next", 1.149,
  
  "GDP", "WEO", "MeanEst_VARunb", "Fall_current", 0.261,
  "GDP", "WEO", "MeanEst_VARunb", "Spring_current", 0.443,
  "GDP", "WEO", "MeanEst_VARunb", "Fall_next", 0.894,
  "GDP", "WEO", "MeanEst_VARunb", "Spring_next", 1.124
)

#==========================================================

#Random Walk CPI data
rw_cpi <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "CPI", "RW", "Mean0_VARml", "Fall_current", 0.087,
  "CPI", "RW", "Mean0_VARml", "Spring_current", 0.428,
  "CPI", "RW", "Mean0_VARml", "Fall_next", 0.849,
  "CPI", "RW", "Mean0_VARml", "Spring_next", 1.058,
  
  # Mean estimated
  "CPI", "RW", "MeanEst_VARml", "Fall_current", 0.089,
  "CPI", "RW", "MeanEst_VARml", "Spring_current", 0.356,
  "CPI", "RW", "MeanEst_VARml", "Fall_next", 0.808,
  "CPI", "RW", "MeanEst_VARml", "Spring_next", 0.984,
  
  # Mean = 0, unbiased VAR
  "CPI", "RW", "Mean0_VARunb", "Fall_current", 0.088,
  "CPI", "RW", "Mean0_VARunb", "Spring_current", 0.426,
  "CPI", "RW", "Mean0_VARunb", "Fall_next", 0.846,
  "CPI", "RW", "Mean0_VARunb", "Spring_next", 1.053,
  
  # Mean estimated, unbiased VAR
  "CPI", "RW", "MeanEst_VARunb", "Fall_current", 0.089,
  "CPI", "RW", "MeanEst_VARunb", "Spring_current", 0.355,
  "CPI", "RW", "MeanEst_VARunb", "Fall_next", 0.807,
  "CPI", "RW", "MeanEst_VARunb", "Spring_next", 0.981
)

#Random Walk GDP data
rw_gdp <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "GDP", "RW", "Mean0_VARml", "Fall_current", 0.102,
  "GDP", "RW", "Mean0_VARml", "Spring_current", 0.869,
  "GDP", "RW", "Mean0_VARml", "Fall_next", 1.095,
  "GDP", "RW", "Mean0_VARml", "Spring_next", 2.539,
  
  # Mean estimated
  "GDP", "RW", "MeanEst_VARml", "Fall_current", 0.106,
  "GDP", "RW", "MeanEst_VARml", "Spring_current", 0.915,
  "GDP", "RW", "MeanEst_VARml", "Fall_next", 1.129,
  "GDP", "RW", "MeanEst_VARml", "Spring_next", 2.640,
  
  # Mean = 0, unbiased VAR
  "GDP", "RW", "Mean0_VARunb", "Fall_current", 0.101,
  "GDP", "RW", "Mean0_VARunb", "Spring_current", 0.868,
  "GDP", "RW", "Mean0_VARunb", "Fall_next", 1.091,
  "GDP", "RW", "Mean0_VARunb", "Spring_next", 2.542,
  
  # Mean estimated, unbiased VAR
  "GDP", "RW", "MeanEst_VARunb", "Fall_current", 0.105,
  "GDP", "RW", "MeanEst_VARunb", "Spring_current", 0.914,
  "GDP", "RW", "MeanEst_VARunb", "Fall_next", 1.124,
  "GDP", "RW", "MeanEst_VARunb", "Spring_next", 2.641
)

#==========================================================

#AR(1) CPI data
ar1_cpi <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "CPI", "AR1", "Mean0_VARml", "Fall_current", 0.094,
  "CPI", "AR1", "Mean0_VARml", "Spring_current", 0.288,
  "CPI", "AR1", "Mean0_VARml", "Fall_next", 0.938,
  "CPI", "AR1", "Mean0_VARml", "Spring_next", 1.481,
  
  # Mean estimated
  "CPI", "AR1", "MeanEst_VARml", "Fall_current", 0.079,
  "CPI", "AR1", "MeanEst_VARml", "Spring_current", 0.257,
  "CPI", "AR1", "MeanEst_VARml", "Fall_next", 0.569,
  "CPI", "AR1", "MeanEst_VARml", "Spring_next", 0.558,
  
  # Mean = 0, VAR unbiased
  "CPI", "AR1", "Mean0_VARunb", "Fall_current", 0.093,
  "CPI", "AR1", "Mean0_VARunb", "Spring_current", 0.287,
  "CPI", "AR1", "Mean0_VARunb", "Fall_next", 0.933,
  "CPI", "AR1", "Mean0_VARunb", "Spring_next", 1.470,
  
  # Mean estimated, VAR unbiased
  "CPI", "AR1", "MeanEst_VARunb", "Fall_current", 0.079,
  "CPI", "AR1", "MeanEst_VARunb", "Spring_current", 0.257,
  "CPI", "AR1", "MeanEst_VARunb", "Fall_next", 0.568,
  "CPI", "AR1", "MeanEst_VARunb", "Spring_next", 0.557
)

#AR(1) GDP data
ar1_gdp <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "GDP", "AR1", "Mean0_VARml", "Fall_current", 0.120,
  "GDP", "AR1", "Mean0_VARml", "Spring_current", 0.573,
  "GDP", "AR1", "Mean0_VARml", "Fall_next", 1.376,
  "GDP", "AR1", "Mean0_VARml", "Spring_next", 1.676,
  
  # Mean estimated
  "GDP", "AR1", "MeanEst_VARml", "Fall_current", 0.107,
  "GDP", "AR1", "MeanEst_VARml", "Spring_current", 0.462,
  "GDP", "AR1", "MeanEst_VARml", "Fall_next", 1.001,
  "GDP", "AR1", "MeanEst_VARml", "Spring_next", 1.161,
  
  # Mean = 0, VAR unbiased
  "GDP", "AR1", "Mean0_VARunb", "Fall_current", 0.120,
  "GDP", "AR1", "Mean0_VARunb", "Spring_current", 0.570,
  "GDP", "AR1", "Mean0_VARunb", "Fall_next", 1.370,
  "GDP", "AR1", "Mean0_VARunb", "Spring_next", 1.668,
  
  # Mean estimated, VAR unbiased
  "GDP", "AR1", "MeanEst_VARunb", "Fall_current", 0.108,
  "GDP", "AR1", "MeanEst_VARunb", "Spring_current", 0.462,
  "GDP", "AR1", "MeanEst_VARunb", "Fall_next", 1.001,
  "GDP", "AR1", "MeanEst_VARunb", "Spring_next", 1.163
)

#==========================================================

#ARIMA(1,1,0) CPI data
arima110_cpi <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "CPI", "ARIMA(1,1,0)", "Mean0_VARml", "Fall_current", 0.089,
  "CPI", "ARIMA(1,1,0)", "Mean0_VARml", "Spring_current", 0.389,
  "CPI", "ARIMA(1,1,0)", "Mean0_VARml", "Fall_next", 0.729,
  "CPI", "ARIMA(1,1,0)", "Mean0_VARml", "Spring_next", 0.961,
  
  # Mean estimated
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARml", "Fall_current", 0.086,
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARml", "Spring_current", 0.337,
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARml", "Fall_next", 0.740,
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARml", "Spring_next", 0.942,
  
  # Mean = 0, VAR unbiased
  "CPI", "ARIMA(1,1,0)", "Mean0_VARunb", "Fall_current", 0.090,
  "CPI", "ARIMA(1,1,0)", "Mean0_VARunb", "Spring_current", 0.387,
  "CPI", "ARIMA(1,1,0)", "Mean0_VARunb", "Fall_next", 0.729,
  "CPI", "ARIMA(1,1,0)", "Mean0_VARunb", "Spring_next", 0.959,
  
  # Mean estimated, VAR unbiased
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARunb", "Fall_current", 0.086,
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARunb", "Spring_current", 0.336,
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARunb", "Fall_next", 0.739,
  "CPI", "ARIMA(1,1,0)", "MeanEst_VARunb", "Spring_next", 0.939
)

#ARIMA(1,1,0) GDP data
arima110_gdp <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "GDP", "ARIMA(1,1,0)", "Mean0_VARml", "Fall_current", 0.092,
  "GDP", "ARIMA(1,1,0)", "Mean0_VARml", "Spring_current", 0.771,
  "GDP", "ARIMA(1,1,0)", "Mean0_VARml", "Fall_next", 1.376,
  "GDP", "ARIMA(1,1,0)", "Mean0_VARml", "Spring_next", 1.676,
  
  # Mean estimated
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARml", "Fall_current", 0.096,
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARml", "Spring_current", 0.815,
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARml", "Fall_next", 1.037,
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARml", "Spring_next", 2.404,
  
  # Mean = 0, VAR unbiased
  "GDP", "ARIMA(1,1,0)", "Mean0_VARunb", "Fall_current", 0.092,
  "GDP", "ARIMA(1,1,0)", "Mean0_VARunb", "Spring_current", 0.771,
  "GDP", "ARIMA(1,1,0)", "Mean0_VARunb", "Fall_next", 0.998,
  "GDP", "ARIMA(1,1,0)", "Mean0_VARunb", "Spring_next", 2.322,
  
  # Mean estimated, VAR unbiased
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARunb", "Fall_current", 0.096,
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARunb", "Spring_current", 0.814,
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARunb", "Fall_next", 1.032,
  "GDP", "ARIMA(1,1,0)", "MeanEst_VARunb", "Spring_next", 2.404
)

#==========================================================

#ARIMA BIC CPI data
arima_bic_cpi <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "CPI", "ARIMA_BIC", "Mean0_VARml", "Fall_current", 0.082,
  "CPI", "ARIMA_BIC", "Mean0_VARml", "Spring_current", 0.259,
  "CPI", "ARIMA_BIC", "Mean0_VARml", "Fall_next", 0.640,
  "CPI", "ARIMA_BIC", "Mean0_VARml", "Spring_next", 0.643,
  
  # Mean estimated
  "CPI", "ARIMA_BIC", "MeanEst_VARml", "Fall_current", 0.085,
  "CPI", "ARIMA_BIC", "MeanEst_VARml", "Spring_current", 0.269,
  "CPI", "ARIMA_BIC", "MeanEst_VARml", "Fall_next", 0.683,
  "CPI", "ARIMA_BIC", "MeanEst_VARml", "Spring_next", 0.696,
  
  # Mean = 0, VAR unbiased
  "CPI", "ARIMA_BIC", "Mean0_VARunb", "Fall_current", 0.082,
  "CPI", "ARIMA_BIC", "Mean0_VARunb", "Spring_current", 0.243,
  "CPI", "ARIMA_BIC", "Mean0_VARunb", "Fall_next", 0.661,
  "CPI", "ARIMA_BIC", "Mean0_VARunb", "Spring_next", 0.653,
  
  # Mean estimated, VAR unbiased
  "CPI", "ARIMA_BIC", "MeanEst_VARunb", "Fall_current", 0.085,
  "CPI", "ARIMA_BIC", "MeanEst_VARunb", "Spring_current", 0.246,
  "CPI", "ARIMA_BIC", "MeanEst_VARunb", "Fall_next", 0.706,
  "CPI", "ARIMA_BIC", "MeanEst_VARunb", "Spring_next", 0.707
)

#ARIMA BIC GDP data
arima_bic_gdp <- tribble(
  ~Target, ~Dataset, ~Method, ~Horizon, ~WIS,
  
  # Mean = 0
  "GDP", "ARIMA_BIC", "Mean0_VARml", "Fall_current", 0.108,
  "GDP", "ARIMA_BIC", "Mean0_VARml", "Spring_current", 0.488,
  "GDP", "ARIMA_BIC", "Mean0_VARml", "Fall_next", 1.057,
  "GDP", "ARIMA_BIC", "Mean0_VARml", "Spring_next", 1.316,
  
  # Mean estimated
  "GDP", "ARIMA_BIC", "MeanEst_VARml", "Fall_current", 0.113,
  "GDP", "ARIMA_BIC", "MeanEst_VARml", "Spring_current", 0.502,
  "GDP", "ARIMA_BIC", "MeanEst_VARml", "Fall_next", 1.084,
  "GDP", "ARIMA_BIC", "MeanEst_VARml", "Spring_next", 1.342,
  
  # Mean = 0, VAR unbiased
  "GDP", "ARIMA_BIC", "Mean0_VARunb", "Fall_current", 0.109,
  "GDP", "ARIMA_BIC", "Mean0_VARunb", "Spring_current", 0.488,
  "GDP", "ARIMA_BIC", "Mean0_VARunb", "Fall_next", 1.058,
  "GDP", "ARIMA_BIC", "Mean0_VARunb", "Spring_next", 1.317,
  
  # Mean estimated, VAR unbiased
  "GDP", "ARIMA_BIC", "MeanEst_VARunb", "Fall_current", 0.113,
  "GDP", "ARIMA_BIC", "MeanEst_VARunb", "Spring_current", 0.501,
  "GDP", "ARIMA_BIC", "MeanEst_VARunb", "Fall_next", 1.084,
  "GDP", "ARIMA_BIC", "MeanEst_VARunb", "Spring_next", 1.342
)


#=========================================================

#remove outliers (mean reversion of AR(1))
ar1_cpi_filtered <- ar1_cpi %>% filter(!grepl("^Mean0", Method))
ar1_gdp_filtered <- ar1_gdp %>% filter(!grepl("^Mean0", Method))


# Combine
data_all <- bind_rows(
  arima_bic_cpi, arima_bic_gdp, 
  arima110_cpi, arima110_gdp, 
  ar1_cpi, ar1_gdp, 
  rw_cpi, rw_gdp, 
  weo_cpi, weo_gdp
)

#WIS mean summary over horizon dataset target combination
#and average relative absolute deviation for dataset target combination

WIS_eval <- data_all %>% 
  group_by(Dataset, Horizon, Target) %>% 
  mutate(
    mean_WIS = mean(WIS),
    WIS_rel_MAD = mean(abs(WIS - mean_WIS)) / mean_WIS,
    WIS_max_rel_dev = max(abs(WIS - mean_WIS)) / mean_WIS
  ) %>% 
  ungroup() %>%
  group_by(Dataset, Target) %>%
  mutate(
    WIS_rel_dispersion_target = mean(WIS_rel_MAD)
  ) %>% 
  ungroup()

WIS_eval %>% 
  group_by(Dataset, Target) %>%
  summarise(
    avg_rel_abs_dev = first(WIS_rel_dispersion_target),
    .groups = "drop"
  )

#finding best method
WIS_best <- data_all %>% 
  group_by(Target, Dataset, Method) %>% 
  mutate(
    WIS_sum = sum(WIS)
  ) %>%
  ungroup() %>%
  group_by(Target, Dataset) %>%
  mutate(
    Method_best = Method[which.min(WIS_sum)]
  ) %>% 
  ungroup()

WIS_best %>%
  group_by(Target, Dataset) %>%
  summarise(
    Method_best = first(Method_best),
    .groups = "drop"
  )
#order for horizons
data_all$Horizon <- factor(
  data_all$Horizon,
  levels = c("Fall_current", "Spring_current",
             "Fall_next", "Spring_next")
)

#=============================================================================
#absolute WIS for selected dataset over all horizons and methods

selected_dataset <- "WEO"

data_filtered <- data_all %>%
  filter(Dataset == selected_dataset) %>%
  mutate(
    Horizon = factor(Horizon, levels = c("Fall_current", "Spring_current",
                                         "Fall_next", "Spring_next")),
    # Recode to nicer x-axis labels
    Horizon = fct_recode(Horizon,
                         "Fall, current"   = "Fall_current",
                         "Spring, current" = "Spring_current",
                         "Fall, next"      = "Fall_next",
                         "Spring, next"    = "Spring_next"),
    Target = factor(Target, levels = c("CPI", "GDP")),
    Method = factor(Method)
  ) %>%
  droplevels()

# Compute mean WIS per horizon (optional horizontal lines)
horizon_means <- data_filtered %>%
  group_by(Target, Horizon) %>%
  summarise(mean_WIS = mean(WIS, na.rm = TRUE), .groups = "drop")

# Plot with lines and points
ggplot(data_filtered,
       aes(x = Horizon, y = WIS, color = Method, shape = Method, group = Method, linetype = Method)) +
  
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
  filter(Dataset == selected_dataset) %>%
  mutate(
    Horizon = factor(Horizon, levels = c("Fall_current", "Spring_current",
                                         "Fall_next", "Spring_next")),
    Horizon = fct_recode(Horizon,
                         "Fall, current"   = "Fall_current",
                         "Spring, current" = "Spring_current",
                         "Fall, next"      = "Fall_next",
                         "Spring, next"    = "Spring_next"),
    Target = factor(Target, levels = c("CPI", "GDP")),
    Method = factor(Method)
  ) %>%
  group_by(Target, Horizon) %>%
  mutate(rel_WIS = WIS / mean(WIS, na.rm = TRUE)) %>%
  ungroup() %>%
  droplevels()




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

desired_levels <- c("WEO", "RW", "AR1", "ARIMA(1,1,0)", "ARIMA_BIC")

wis_h1 <- data_all %>%
  mutate(
    # Keep Horizon in your preferred internal order
    Horizon = factor(Horizon, levels = c("Fall_current", "Spring_current",
                                         "Fall_next", "Spring_next")),
    # Recode to nicer x-axis labels
    Horizon = fct_recode(Horizon,
                         "Fall, current"   = "Fall_current",
                         "Spring, current" = "Spring_current",
                         "Fall, next"      = "Fall_next",
                         "Spring, next"    = "Spring_next"),
    Target = factor(Target, levels = c("CPI", "GDP")),
    Method = factor(Method),
    # <<< Enforce Dataset order here >>>
    Dataset = fct_relevel(Dataset, desired_levels)
  ) %>%
  filter(Horizon == selected_horizon) %>%
  droplevels()



ggplot(
  wis_h1,
  aes(x = Dataset, y = WIS, fill = Method)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "white"
  ) +
  facet_wrap(~ Target, nrow = 1, scales = "fixed") +
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

#=================================================================
#coverage data

coverage_weo <- tribble(
  ~Dataset, ~Target, ~Method, ~Horizon, ~Alpha, ~Coverage,
  
  # =======================
  # CPI — mean = 0
  # =======================
  "WEO", "CPI", "Mean0_VARml", "Fall_current",   0.5, 0.571,
  "WEO", "CPI", "Mean0_VARml", "Fall_current",   0.8, 0.798,
  "WEO", "CPI", "Mean0_VARml", "Spring_current", 0.5, 0.405,
  "WEO", "CPI", "Mean0_VARml", "Spring_current", 0.8, 0.726,
  "WEO", "CPI", "Mean0_VARml", "Fall_next",      0.5, 0.393,
  "WEO", "CPI", "Mean0_VARml", "Fall_next",      0.8, 0.702,
  "WEO", "CPI", "Mean0_VARml", "Spring_next",    0.5, 0.393,
  "WEO", "CPI", "Mean0_VARml", "Spring_next",    0.8, 0.679,
  
  # =======================
  # CPI — mean estimated
  # =======================
  "WEO", "CPI", "MeanEst_VARml", "Fall_current",   0.5, 0.476,
  "WEO", "CPI", "MeanEst_VARml", "Fall_current",   0.8, 0.786,
  "WEO", "CPI", "MeanEst_VARml", "Spring_current", 0.5, 0.440,
  "WEO", "CPI", "MeanEst_VARml", "Spring_current", 0.8, 0.738,
  "WEO", "CPI", "MeanEst_VARml", "Fall_next",      0.5, 0.405,
  "WEO", "CPI", "MeanEst_VARml", "Fall_next",      0.8, 0.702,
  "WEO", "CPI", "MeanEst_VARml", "Spring_next",    0.5, 0.417,
  "WEO", "CPI", "MeanEst_VARml", "Spring_next",    0.8, 0.631,
  
  # =======================
  # CPI — mean = 0, VAR unb
  # =======================
  "WEO", "CPI", "Mean0_VARunb", "Fall_current",   0.5, 0.571,
  "WEO", "CPI", "Mean0_VARunb", "Fall_current",   0.8, 0.833,
  "WEO", "CPI", "Mean0_VARunb", "Spring_current", 0.5, 0.405,
  "WEO", "CPI", "Mean0_VARunb", "Spring_current", 0.8, 0.774,
  "WEO", "CPI", "Mean0_VARunb", "Fall_next",      0.5, 0.405,
  "WEO", "CPI", "Mean0_VARunb", "Fall_next",      0.8, 0.714,
  "WEO", "CPI", "Mean0_VARunb", "Spring_next",    0.5, 0.417,
  "WEO", "CPI", "Mean0_VARunb", "Spring_next",    0.8, 0.714,
  
  # =======================
  # CPI — mean estimated, VAR unb
  # =======================
  "WEO", "CPI", "MeanEst_VARunb", "Fall_current",   0.5, 0.512,
  "WEO", "CPI", "MeanEst_VARunb", "Fall_current",   0.8, 0.810,
  "WEO", "CPI", "MeanEst_VARunb", "Spring_current", 0.5, 0.476,
  "WEO", "CPI", "MeanEst_VARunb", "Spring_current", 0.8, 0.762,
  "WEO", "CPI", "MeanEst_VARunb", "Fall_next",      0.5, 0.405,
  "WEO", "CPI", "MeanEst_VARunb", "Fall_next",      0.8, 0.702,
  "WEO", "CPI", "MeanEst_VARunb", "Spring_next",    0.5, 0.440,
  "WEO", "CPI", "MeanEst_VARunb", "Spring_next",    0.8, 0.690,
  
  # =======================
  # GDP — mean = 0
  # =======================
  "WEO", "GDP", "Mean0_VARml", "Fall_current",   0.5, 0.560,
  "WEO", "GDP", "Mean0_VARml", "Fall_current",   0.8, 0.798,
  "WEO", "GDP", "Mean0_VARml", "Spring_current", 0.5, 0.500,
  "WEO", "GDP", "Mean0_VARml", "Spring_current", 0.8, 0.786,
  "WEO", "GDP", "Mean0_VARml", "Fall_next",      0.5, 0.512,
  "WEO", "GDP", "Mean0_VARml", "Fall_next",      0.8, 0.750,
  "WEO", "GDP", "Mean0_VARml", "Spring_next",    0.5, 0.464,
  "WEO", "GDP", "Mean0_VARml", "Spring_next",    0.8, 0.702,
  
  # =======================
  # GDP — mean estimated
  # =======================
  "WEO", "GDP", "MeanEst_VARml", "Fall_current",   0.5, 0.512,
  "WEO", "GDP", "MeanEst_VARml", "Fall_current",   0.8, 0.762,
  "WEO", "GDP", "MeanEst_VARml", "Spring_current", 0.5, 0.524,
  "WEO", "GDP", "MeanEst_VARml", "Spring_current", 0.8, 0.762,
  "WEO", "GDP", "MeanEst_VARml", "Fall_next",      0.5, 0.440,
  "WEO", "GDP", "MeanEst_VARml", "Fall_next",      0.8, 0.738,
  "WEO", "GDP", "MeanEst_VARml", "Spring_next",    0.5, 0.429,
  "WEO", "GDP", "MeanEst_VARml", "Spring_next",    0.8, 0.690,
  
  # =======================
  # GDP — mean = 0, VAR unb
  # =======================
  "WEO", "GDP", "Mean0_VARunb", "Fall_current",   0.5, 0.571,
  "WEO", "GDP", "Mean0_VARunb", "Fall_current",   0.8, 0.810,
  "WEO", "GDP", "Mean0_VARunb", "Spring_current", 0.5, 0.536,
  "WEO", "GDP", "Mean0_VARunb", "Spring_current", 0.8, 0.810,
  "WEO", "GDP", "Mean0_VARunb", "Fall_next",      0.5, 0.548,
  "WEO", "GDP", "Mean0_VARunb", "Fall_next",      0.8, 0.750,
  "WEO", "GDP", "Mean0_VARunb", "Spring_next",    0.5, 0.476,
  "WEO", "GDP", "Mean0_VARunb", "Spring_next",    0.8, 0.726,
  
  # =======================
  # GDP — mean estimated, VAR unb
  # =======================
  "WEO", "GDP", "MeanEst_VARunb", "Fall_current",   0.5, 0.512,
  "WEO", "GDP", "MeanEst_VARunb", "Fall_current",   0.8, 0.798,
  "WEO", "GDP", "MeanEst_VARunb", "Spring_current", 0.5, 0.595,
  "WEO", "GDP", "MeanEst_VARunb", "Spring_current", 0.8, 0.774,
  "WEO", "GDP", "MeanEst_VARunb", "Fall_next",      0.5, 0.452,
  "WEO", "GDP", "MeanEst_VARunb", "Fall_next",      0.8, 0.786,
  "WEO", "GDP", "MeanEst_VARunb", "Spring_next",    0.5, 0.452,
  "WEO", "GDP", "MeanEst_VARunb", "Spring_next",    0.8, 0.714
)

#==========================================================

coverage_rw <- tribble(
  ~Dataset, ~Target, ~Method, ~Horizon, ~Alpha, ~Coverage,
  
  # ==================================================
  # CPI — RW dataset
  # ==================================================
  
  # Mean0_VARml
  "RW", "CPI", "Mean0_VARml", "Fall_current",   0.5, 0.452,
  "RW", "CPI", "Mean0_VARml", "Fall_current",   0.8, 0.667,
  "RW", "CPI", "Mean0_VARml", "Spring_current", 0.5, 0.381,
  "RW", "CPI", "Mean0_VARml", "Spring_current", 0.8, 0.607,
  "RW", "CPI", "Mean0_VARml", "Fall_next",      0.5, 0.405,
  "RW", "CPI", "Mean0_VARml", "Fall_next",      0.8, 0.607,
  "RW", "CPI", "Mean0_VARml", "Spring_next",    0.5, 0.417,
  "RW", "CPI", "Mean0_VARml", "Spring_next",    0.8, 0.655,
  
  # MeanEst_VARml
  "RW", "CPI", "MeanEst_VARml", "Fall_current",   0.5, 0.417,
  "RW", "CPI", "MeanEst_VARml", "Fall_current",   0.8, 0.595,
  "RW", "CPI", "MeanEst_VARml", "Spring_current", 0.5, 0.417,
  "RW", "CPI", "MeanEst_VARml", "Spring_current", 0.8, 0.714,
  "RW", "CPI", "MeanEst_VARml", "Fall_next",      0.5, 0.429,
  "RW", "CPI", "MeanEst_VARml", "Fall_next",      0.8, 0.679,
  "RW", "CPI", "MeanEst_VARml", "Spring_next",    0.5, 0.417,
  "RW", "CPI", "MeanEst_VARml", "Spring_next",    0.8, 0.679,
  
  # Mean0_VARunb
  "RW", "CPI", "Mean0_VARunb", "Fall_current",   0.5, 0.464,
  "RW", "CPI", "Mean0_VARunb", "Fall_current",   0.8, 0.679,
  "RW", "CPI", "Mean0_VARunb", "Spring_current", 0.5, 0.393,
  "RW", "CPI", "Mean0_VARunb", "Spring_current", 0.8, 0.607,
  "RW", "CPI", "Mean0_VARunb", "Fall_next",      0.5, 0.417,
  "RW", "CPI", "Mean0_VARunb", "Fall_next",      0.8, 0.619,
  "RW", "CPI", "Mean0_VARunb", "Spring_next",    0.5, 0.429,
  "RW", "CPI", "Mean0_VARunb", "Spring_next",    0.8, 0.655,
  
  # MeanEst_VARunb
  "RW", "CPI", "MeanEst_VARunb", "Fall_current",   0.5, 0.429,
  "RW", "CPI", "MeanEst_VARunb", "Fall_current",   0.8, 0.619,
  "RW", "CPI", "MeanEst_VARunb", "Spring_current", 0.5, 0.440,
  "RW", "CPI", "MeanEst_VARunb", "Spring_current", 0.8, 0.738,
  "RW", "CPI", "MeanEst_VARunb", "Fall_next",      0.5, 0.440,
  "RW", "CPI", "MeanEst_VARunb", "Fall_next",      0.8, 0.702,
  "RW", "CPI", "MeanEst_VARunb", "Spring_next",    0.5, 0.452,
  "RW", "CPI", "MeanEst_VARunb", "Spring_next",    0.8, 0.679,
  
  # ==================================================
  # GDP — RW dataset
  # ==================================================
  
  # Mean0_VARml
  "RW", "GDP", "Mean0_VARml", "Fall_current",   0.5, 0.405,
  "RW", "GDP", "Mean0_VARml", "Fall_current",   0.8, 0.667,
  "RW", "GDP", "Mean0_VARml", "Spring_current", 0.5, 0.464,
  "RW", "GDP", "Mean0_VARml", "Spring_current", 0.8, 0.762,
  "RW", "GDP", "Mean0_VARml", "Fall_next",      0.5, 0.345,
  "RW", "GDP", "Mean0_VARml", "Fall_next",      0.8, 0.726,
  "RW", "GDP", "Mean0_VARml", "Spring_next",    0.5, 0.500,
  "RW", "GDP", "Mean0_VARml", "Spring_next",    0.8, 0.750,
  
  # MeanEst_VARml
  "RW", "GDP", "MeanEst_VARml", "Fall_current",   0.5, 0.417,
  "RW", "GDP", "MeanEst_VARml", "Fall_current",   0.8, 0.679,
  "RW", "GDP", "MeanEst_VARml", "Spring_current", 0.5, 0.476,
  "RW", "GDP", "MeanEst_VARml", "Spring_current", 0.8, 0.702,
  "RW", "GDP", "MeanEst_VARml", "Fall_next",      0.5, 0.369,
  "RW", "GDP", "MeanEst_VARml", "Fall_next",      0.8, 0.714,
  "RW", "GDP", "MeanEst_VARml", "Spring_next",    0.5, 0.488,
  "RW", "GDP", "MeanEst_VARml", "Spring_next",    0.8, 0.762,
  
  # Mean0_VARunb
  "RW", "GDP", "Mean0_VARunb", "Fall_current",   0.5, 0.440,
  "RW", "GDP", "Mean0_VARunb", "Fall_current",   0.8, 0.690,
  "RW", "GDP", "Mean0_VARunb", "Spring_current", 0.5, 0.476,
  "RW", "GDP", "Mean0_VARunb", "Spring_current", 0.8, 0.786,
  "RW", "GDP", "Mean0_VARunb", "Fall_next",      0.5, 0.357,
  "RW", "GDP", "Mean0_VARunb", "Fall_next",      0.8, 0.762,
  "RW", "GDP", "Mean0_VARunb", "Spring_next",    0.5, 0.536,
  "RW", "GDP", "Mean0_VARunb", "Spring_next",    0.8, 0.786,
  
  # MeanEst_VARunb
  "RW", "GDP", "MeanEst_VARunb", "Fall_current",   0.5, 0.417,
  "RW", "GDP", "MeanEst_VARunb", "Fall_current",   0.8, 0.702,
  "RW", "GDP", "MeanEst_VARunb", "Spring_current", 0.5, 0.500,
  "RW", "GDP", "MeanEst_VARunb", "Spring_current", 0.8, 0.726,
  "RW", "GDP", "MeanEst_VARunb", "Fall_next",      0.5, 0.381,
  "RW", "GDP", "MeanEst_VARunb", "Fall_next",      0.8, 0.750,
  "RW", "GDP", "MeanEst_VARunb", "Spring_next",    0.5, 0.524,
  "RW", "GDP", "MeanEst_VARunb", "Spring_next",    0.8, 0.774
)

coverage_ar1 <- tribble(
  ~Dataset, ~Target, ~Method, ~Horizon, ~Alpha, ~Coverage,
  
  # ==================================================
  # CPI — AR(1) dataset
  # ==================================================
  
  # Mean0_VARml
  "AR1", "CPI", "Mean0_VARml", "Fall_current",   0.5, 0.345,
  "AR1", "CPI", "Mean0_VARml", "Fall_current",   0.8, 0.607,
  "AR1", "CPI", "Mean0_VARml", "Spring_current", 0.5, 0.298,
  "AR1", "CPI", "Mean0_VARml", "Spring_current", 0.8, 0.667,
  "AR1", "CPI", "Mean0_VARml", "Fall_next",      0.5, 0.298,
  "AR1", "CPI", "Mean0_VARml", "Fall_next",      0.8, 0.464,
  "AR1", "CPI", "Mean0_VARml", "Spring_next",    0.5, 0.131,
  "AR1", "CPI", "Mean0_VARml", "Spring_next",    0.8, 0.274,
  
  # MeanEst_VARml
  "AR1", "CPI", "MeanEst_VARml", "Fall_current",   0.5, 0.429,
  "AR1", "CPI", "MeanEst_VARml", "Fall_current",   0.8, 0.619,
  "AR1", "CPI", "MeanEst_VARml", "Spring_current", 0.5, 0.417,
  "AR1", "CPI", "MeanEst_VARml", "Spring_current", 0.8, 0.726,
  "AR1", "CPI", "MeanEst_VARml", "Fall_next",      0.5, 0.440,
  "AR1", "CPI", "MeanEst_VARml", "Fall_next",      0.8, 0.667,
  "AR1", "CPI", "MeanEst_VARml", "Spring_next",    0.5, 0.429,
  "AR1", "CPI", "MeanEst_VARml", "Spring_next",    0.8, 0.714,
  
  # Mean0_VARunb
  "AR1", "CPI", "Mean0_VARunb", "Fall_current",   0.5, 0.345,
  "AR1", "CPI", "Mean0_VARunb", "Fall_current",   0.8, 0.619,
  "AR1", "CPI", "Mean0_VARunb", "Spring_current", 0.5, 0.310,
  "AR1", "CPI", "Mean0_VARunb", "Spring_current", 0.8, 0.690,
  "AR1", "CPI", "Mean0_VARunb", "Fall_next",      0.5, 0.298,
  "AR1", "CPI", "Mean0_VARunb", "Fall_next",      0.8, 0.464,
  "AR1", "CPI", "Mean0_VARunb", "Spring_next",    0.5, 0.131,
  "AR1", "CPI", "Mean0_VARunb", "Spring_next",    0.8, 0.274,
  
  # MeanEst_VARunb
  "AR1", "CPI", "MeanEst_VARunb", "Fall_current",   0.5, 0.440,
  "AR1", "CPI", "MeanEst_VARunb", "Fall_current",   0.8, 0.631,
  "AR1", "CPI", "MeanEst_VARunb", "Spring_current", 0.5, 0.440,
  "AR1", "CPI", "MeanEst_VARunb", "Spring_current", 0.8, 0.726,
  "AR1", "CPI", "MeanEst_VARunb", "Fall_next",      0.5, 0.452,
  "AR1", "CPI", "MeanEst_VARunb", "Fall_next",      0.8, 0.690,
  "AR1", "CPI", "MeanEst_VARunb", "Spring_next",    0.5, 0.476,
  "AR1", "CPI", "MeanEst_VARunb", "Spring_next",    0.8, 0.738,
  
  
  # ==================================================
  # GDP — AR(1) dataset
  # ==================================================
  
  # Mean0_VARml
  "AR1", "GDP", "Mean0_VARml", "Fall_current",   0.5, 0.488,
  "AR1", "GDP", "Mean0_VARml", "Fall_current",   0.8, 0.714,
  "AR1", "GDP", "Mean0_VARml", "Spring_current", 0.5, 0.298,
  "AR1", "GDP", "Mean0_VARml", "Spring_current", 0.8, 0.607,
  "AR1", "GDP", "Mean0_VARml", "Fall_next",      0.5, 0.381,
  "AR1", "GDP", "Mean0_VARml", "Fall_next",      0.8, 0.524,
  "AR1", "GDP", "Mean0_VARml", "Spring_next",    0.5, 0.357,
  "AR1", "GDP", "Mean0_VARml", "Spring_next",    0.8, 0.512,
  
  # MeanEst_VARml
  "AR1", "GDP", "MeanEst_VARml", "Fall_current",   0.5, 0.536,
  "AR1", "GDP", "MeanEst_VARml", "Fall_current",   0.8, 0.821,
  "AR1", "GDP", "MeanEst_VARml", "Spring_current", 0.5, 0.512,
  "AR1", "GDP", "MeanEst_VARml", "Spring_current", 0.8, 0.774,
  "AR1", "GDP", "MeanEst_VARml", "Fall_next",      0.5, 0.524,
  "AR1", "GDP", "MeanEst_VARml", "Fall_next",      0.8, 0.750,
  "AR1", "GDP", "MeanEst_VARml", "Spring_next",    0.5, 0.583,
  "AR1", "GDP", "MeanEst_VARml", "Spring_next",    0.8, 0.738,
  
  # Mean0_VARunb
  "AR1", "GDP", "Mean0_VARunb", "Fall_current",   0.5, 0.488,
  "AR1", "GDP", "Mean0_VARunb", "Fall_current",   0.8, 0.750,
  "AR1", "GDP", "Mean0_VARunb", "Spring_current", 0.5, 0.310,
  "AR1", "GDP", "Mean0_VARunb", "Spring_current", 0.8, 0.667,
  "AR1", "GDP", "Mean0_VARunb", "Fall_next",      0.5, 0.405,
  "AR1", "GDP", "Mean0_VARunb", "Fall_next",      0.8, 0.560,
  "AR1", "GDP", "Mean0_VARunb", "Spring_next",    0.5, 0.369,
  "AR1", "GDP", "Mean0_VARunb", "Spring_next",    0.8, 0.536,
  
  # MeanEst_VARunb
  "AR1", "GDP", "MeanEst_VARunb", "Fall_current",   0.5, 0.595,
  "AR1", "GDP", "MeanEst_VARunb", "Fall_current",   0.8, 0.833,
  "AR1", "GDP", "MeanEst_VARunb", "Spring_current", 0.5, 0.536,
  "AR1", "GDP", "MeanEst_VARunb", "Spring_current", 0.8, 0.798,
  "AR1", "GDP", "MeanEst_VARunb", "Fall_next",      0.5, 0.536,
  "AR1", "GDP", "MeanEst_VARunb", "Fall_next",      0.8, 0.786,
  "AR1", "GDP", "MeanEst_VARunb", "Spring_next",    0.5, 0.583,
  "AR1", "GDP", "MeanEst_VARunb", "Spring_next",    0.8, 0.762
)


coverage_arima110 <- tribble(
  ~Dataset, ~Target, ~Method, ~Horizon, ~Alpha, ~Coverage,
  
  # ==================================================
  # CPI — ARIMA(1,1,0)
  # ==================================================
  
  # Mean0_VARml
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Fall_current",   0.5, 0.464,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Fall_current",   0.8, 0.667,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Spring_current", 0.5, 0.405,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Spring_current", 0.8, 0.595,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Fall_next",      0.5, 0.488,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Fall_next",      0.8, 0.726,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Spring_next",    0.5, 0.405,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARml", "Spring_next",    0.8, 0.643,
  
  # MeanEst_VARml
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Fall_current",   0.5, 0.417,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Fall_current",   0.8, 0.690,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Spring_current", 0.5, 0.393,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Spring_current", 0.8, 0.655,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Fall_next",      0.5, 0.429,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Fall_next",      0.8, 0.714,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Spring_next",    0.5, 0.452,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARml", "Spring_next",    0.8, 0.702,
  
  # Mean0_VARunb
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Fall_current",   0.5, 0.476,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Fall_current",   0.8, 0.679,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Spring_current", 0.5, 0.417,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Spring_current", 0.8, 0.619,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Fall_next",      0.5, 0.512,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Fall_next",      0.8, 0.726,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Spring_next",    0.5, 0.429,
  "ARIMA(1,1,0)", "CPI", "Mean0_VARunb", "Spring_next",    0.8, 0.643,
  
  # MeanEst_VARunb
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Fall_current",   0.5, 0.440,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Fall_current",   0.8, 0.690,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Spring_current", 0.5, 0.429,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Spring_current", 0.8, 0.667,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Fall_next",      0.5, 0.452,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Fall_next",      0.8, 0.714,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Spring_next",    0.5, 0.452,
  "ARIMA(1,1,0)", "CPI", "MeanEst_VARunb", "Spring_next",    0.8, 0.702,
  
  
  # ==================================================
  # GDP — ARIMA(1,1,0)
  # ==================================================
  
  # Mean0_VARml
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Fall_current",   0.5, 0.488,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Fall_current",   0.8, 0.750,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Spring_current", 0.5, 0.452,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Spring_current", 0.8, 0.762,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Fall_next",      0.5, 0.333,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Fall_next",      0.8, 0.714,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Spring_next",    0.5, 0.512,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARml", "Spring_next",    0.8, 0.714,
  
  # MeanEst_VARml
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Fall_current",   0.5, 0.440,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Fall_current",   0.8, 0.738,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Spring_current", 0.5, 0.429,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Spring_current", 0.8, 0.702,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Fall_next",      0.5, 0.345,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Fall_next",      0.8, 0.690,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Spring_next",    0.5, 0.464,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARml", "Spring_next",    0.8, 0.738,
  
  # Mean0_VARunb
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Fall_current",   0.5, 0.488,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Fall_current",   0.8, 0.774,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Spring_current", 0.5, 0.476,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Spring_current", 0.8, 0.774,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Fall_next",      0.5, 0.381,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Fall_next",      0.8, 0.714,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Spring_next",    0.5, 0.548,
  "ARIMA(1,1,0)", "GDP", "Mean0_VARunb", "Spring_next",    0.8, 0.738,
  
  # MeanEst_VARunb
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Fall_current",   0.5, 0.452,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Fall_current",   0.8, 0.762,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Spring_current", 0.5, 0.440,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Spring_current", 0.8, 0.750,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Fall_next",      0.5, 0.345,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Fall_next",      0.8, 0.738,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Spring_next",    0.5, 0.464,
  "ARIMA(1,1,0)", "GDP", "MeanEst_VARunb", "Spring_next",    0.8, 0.750
)


coverage_arima_bic <- tribble(
  ~Dataset, ~Target, ~Method, ~Horizon, ~Alpha, ~Coverage,
  
  # ==================================================
  # CPI — ARIMA BIC
  # ==================================================
  
  # Mean0_VARml
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Fall_current",   0.5, 0.469,
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Fall_current",   0.8, 0.728,
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Spring_current", 0.5, 0.444,
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Spring_current", 0.8, 0.753,
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Fall_next",      0.5, 0.488,
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Fall_next",      0.8, 0.780,
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Spring_next",    0.5, 0.549,
  "ARIMA_BIC", "CPI", "Mean0_VARml", "Spring_next",    0.8, 0.756,
  
  # MeanEst_VARml
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Fall_current",   0.5, 0.407,
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Fall_current",   0.8, 0.630,
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Spring_current", 0.5, 0.457,
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Spring_current", 0.8, 0.716,
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Fall_next",      0.5, 0.463,
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Fall_next",      0.8, 0.707,
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Spring_next",    0.5, 0.476,
  "ARIMA_BIC", "CPI", "MeanEst_VARml", "Spring_next",    0.8, 0.659,
  
  # Mean0_VARunb
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Fall_current",   0.5, 0.480,
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Fall_current",   0.8, 0.733,
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Spring_current", 0.5, 0.432,
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Spring_current", 0.8, 0.770,
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Fall_next",      0.5, 0.507,
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Fall_next",      0.8, 0.773,
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Spring_next",    0.5, 0.514,
  "ARIMA_BIC", "CPI", "Mean0_VARunb", "Spring_next",    0.8, 0.743,
  
  # MeanEst_VARunb
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Fall_current",   0.5, 0.427,
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Fall_current",   0.8, 0.627,
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Spring_current", 0.5, 0.486,
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Spring_current", 0.8, 0.716,
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Fall_next",      0.5, 0.480,
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Fall_next",      0.8, 0.733,
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Spring_next",    0.5, 0.459,
  "ARIMA_BIC", "CPI", "MeanEst_VARunb", "Spring_next",    0.8, 0.662,
  
  
  # ==================================================
  # GDP — ARIMA BIC
  # ==================================================
  
  # Mean0_VARml
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Fall_current",   0.5, 0.583,
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Fall_current",   0.8, 0.786,
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Spring_current", 0.5, 0.488,
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Spring_current", 0.8, 0.738,
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Fall_next",      0.5, 0.571,
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Fall_next",      0.8, 0.762,
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Spring_next",    0.5, 0.571,
  "ARIMA_BIC", "GDP", "Mean0_VARml", "Spring_next",    0.8, 0.738,
  
  # MeanEst_VARml
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Fall_current",   0.5, 0.560,
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Fall_current",   0.8, 0.786,
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Spring_current", 0.5, 0.440,
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Spring_current", 0.8, 0.714,
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Fall_next",      0.5, 0.500,
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Fall_next",      0.8, 0.738,
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Spring_next",    0.5, 0.536,
  "ARIMA_BIC", "GDP", "MeanEst_VARml", "Spring_next",    0.8, 0.762,
  
  # Mean0_VARunb
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Fall_current",   0.5, 0.595,
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Fall_current",   0.8, 0.810,
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Spring_current", 0.5, 0.500,
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Spring_current", 0.8, 0.762,
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Fall_next",      0.5, 0.583,
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Fall_next",      0.8, 0.786,
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Spring_next",    0.5, 0.571,
  "ARIMA_BIC", "GDP", "Mean0_VARunb", "Spring_next",    0.8, 0.762,
  
  # MeanEst_VARunb
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Fall_current",   0.5, 0.571,
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Fall_current",   0.8, 0.798,
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Spring_current", 0.5, 0.500,
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Spring_current", 0.8, 0.738,
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Fall_next",      0.5, 0.512,
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Fall_next",      0.8, 0.762,
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Spring_next",    0.5, 0.536,
  "ARIMA_BIC", "GDP", "MeanEst_VARunb", "Spring_next",    0.8, 0.774
)

#combine all
coverage_all <- bind_rows(
  coverage_ar1,
  coverage_arima110,
  coverage_arima_bic,
  coverage_weo,
  coverage_rw
)

#==========================================================
#coverage analysis
#==========================================================

coverage_all %>%
  group_by(Method, Target, Alpha) %>%
  summarise(mean_coverage = mean(Coverage), .groups = "drop")

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





