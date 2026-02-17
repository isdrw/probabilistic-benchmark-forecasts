rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)

source("scripts/utilities/utility_functions.R")
source("scripts/utilities/data_transformation_functions.R")


#load dataset with AR(1) point predictions aggregated to annual data (expanding window)
agg_ext_window_data <- load_and_prepare_ARIMA1_0_0_data("data/processed/point predictions/point_predictions_arima1_0_0.csv") %>% 
  aggregate_to_annual_input()


#calculate rolling window quantiles
plot_data_quantiles <- agg_ext_window_data %>%
  filter(country == "JPN",
         horizon == 1.5) %>%
  filter(is.finite(target_year),
         is.finite(pred_cpi),
         is.finite(tv_cpi)) %>%
  arrange(target_year) %>%
  mutate(
    # signed forecast error
    forecast_error = tv_cpi - pred_cpi,
    
    #rolling window standard deviation
    rolling_sd = slide_dbl(
      forecast_error,
      ~ sd(.x, na.rm = TRUE),
      .before = 10,
      .complete = TRUE
    ),
    
    q10 = qnorm(0.1, mean = 0, sd = rolling_sd),
    q90 = qnorm(0.9, mean = 0, sd = rolling_sd),
    
    # interval bounds (on original scale)
    lower_bound = pred_cpi + q10,
    upper_bound = pred_cpi + q90
  )


#plot quantiles
ggplot(plot_data_quantiles, aes(x = target_year)) +
  # Quantile band (10%–90%)
  geom_ribbon(
    aes(ymin = lower_bound,
        ymax = upper_bound),
    fill = "steelblue",
    alpha = 0.25
  ) +
  
  # Forecast & truth
  geom_line(aes(y = tv_cpi), color = "black", linewidth = 0.6) +
  geom_point(aes(y = tv_cpi), color = "black", size = 1.5) +
  geom_point(aes(y = pred_cpi), color = "blue", size = 1.5) +
  
  # Global truth mean
  geom_hline(
    yintercept = mean(plot_data_mean$tv_cpi, na.rm = TRUE),
    linetype = "dotdash",
    linewidth = 0.6,
    color = "black"
  ) +
  
  scale_y_continuous(
    name = "CPI level"
  ) +

  theme_minimal() +
  labs(
    x = "Target year",
    title = "Normal Quantile Band for AR(1) predictions",
  )


#plot data for mean reversion illustration
plot_data_mean <- agg_ext_window_data %>%
  filter(country == "JPN",
         horizon == 1.5) %>%
  filter(is.finite(target_year),
         is.finite(pred_cpi),
         is.finite(tv_cpi)) %>%
  arrange(target_year)



ggplot(plot_data_mean, aes(x = target_year)) +
  # Prediction
  geom_line(aes(y = tv_cpi), color = "black", linewidth = 0.6) +
  geom_point(aes(y = tv_cpi), color = "black", size = 1.5) +
  
  # Truth
  geom_point(aes(y = pred_cpi), color = "blue", size = 1.5) +
  
  # Global truth mean
  geom_hline(
    yintercept = mean(plot_data_mean$tv_cpi, na.rm = TRUE),
    linetype = "dotdash",
    linewidth = 0.6,
    color = "black"
  ) +
  
  
  theme_minimal() +
  labs(
    x = "Target year",
    y = "CPI level",
    title = "Mean Reversion of AR(1) process",
  )


